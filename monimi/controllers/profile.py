import logging
import pytz
from sqlalchemy import *
from sqlalchemy.sql import func
from urllib import urlencode
from monimi.web import jrpc, authorize, ValidationFailedException
import monimi.permissions as perm
from monimi import payment
from monimi import validators
from monimi.util import encrypt_password, get_utc_offset
from monimi.db import t_user

log = logging.getLogger(__name__)

class ProfileController(object):
    def __init__(self, config):
        self.config = config
        self.action = config.get('paypal', 'url')
        common = dict(business=config.get('paypal', 'business'),
                      no_shipping='1',
                      no_note='1',
                      currency_code='USD')
        self.common_sub = dict(cmd='_xclick-subscriptions',
                          p3='1',
                          t3='M',
                          src='1',
                          sra='1')
        self.common_buy = dict(cmd='_xclick')
        self.common_sub.update(common)
        self.common_buy.update(common)
        
    @jrpc()
    @authorize(perm.ValidUser())
    def get_profile(self, r):
        if r.user.subscr_id:
            unsubscribe = '%s?%s' % (self.config.get('paypal', 'url'), 
                                     urlencode([('cmd', '_subscr-find'),
                                                ('alias', self.config.get('paypal', 'business'))]))
        else:
            unsubscribe = None
        return dict(plan=r.user.plan, 
                    sms_credits=r.user.sms_credits_plan + r.user.sms_credits_purchased,
                    unsubscribe_url=unsubscribe,
                    first_name=r.user.first_name,
                    last_name=r.user.last_name,
                    username=r.user.username,
                    email=r.user.email,
                    mobile=r.user.mobile,
                    timezone=r.user.timezone)

    @jrpc(validator=validators.ChangePasswordSchema())
    @authorize(perm.ValidUser())
    def change_password(self, r, old_password, new_password, confirm_password):
        if r.user.password == encrypt_password(old_password):
            r.user.password = encrypt_password(new_password)
            r.session.flush()
        else:
            raise ValidationFailedException(dict(old_password='Old Password is incorrect'))

    @jrpc(validator=validators.ChangeTimezoneSchema())
    @authorize(perm.ValidUser())
    def change_time_zone(self, r, timezone):
        r.user.timezone = timezone
        r.user.utc_offset = get_utc_offset(pytz.timezone(timezone))
        r.session.flush()

    @jrpc(validator=validators.ChangeUserSchema())
    @authorize(perm.ValidUser())
    def change_user(self, r, first_name, last_name, username):
        if r.user.username == None and r.user.username != username:
            # attempting username change
            user_count = r.connection.execute(select([func.count(t_user.c.user_id)],
                                                          func.lower(t_user.c.username) == username.lower())).fetchone()[0]
            if user_count:
                raise ValidationFailedException(dict(username='That username already exists'))
            r.user.username = username
        r.user.first_name = first_name
        r.user.last_name = last_name    
        r.session.flush()

    @jrpc()
    @authorize(perm.ValidUser())
    def payment_buttons(self, r):
        subs = dict(basic=dict(name='Basic Plan', price='4.99'),
                    pro=dict(name='Pro Plan', price='14.50'))

        b_sms1 = dict(item_name='20 SMS Credits', amount='7.00', custom=r.user.user_id, **self.common_buy)
        b_sms2 = dict(item_name='50 SMS Credits', amount='17.00', custom=r.user.user_id, **self.common_buy)

        if r.user.plan == 'free':
            buttons = [self._subs_button(r.user.user_id, **subs['basic']),
                       self._subs_button(r.user.user_id, **subs['pro'])]
        elif r.user.plan == 'basic':
            buttons = [self._upgr_button(r.user.user_id, **subs['pro'])]
        elif r.user.plan == 'pro':
            buttons = []
        
        buttons.append(self._buy_button(r.user.user_id, '20 SMS Credits', '7.00'))
        buttons.append(self._buy_button(r.user.user_id, '50 SMS Credits', '17.00'))

        return buttons

    def _subs_button(self, user_id, name, price):
        pp_order = payment.PaypalOrder(
                        custom=user_id,
                        item_name=name,
                        a3=price,
                        **self.common_sub)
        return dict(action=self.action, name=name,  kind='subscribe',
                    fields=dict(cmd='_s-xclick', encrypted=pp_order.encrypt(self.config)))

    def _upgr_button(self, user_id, name, price):
        pp_order = payment.PaypalOrder(
                        custom=user_id,
                        item_name=name,
                        a3=price,
                        modify='2',
                        **self.common_sub)
        pp_order['return'] = 'http://monimi.net/subscription_modified'
        return dict(action=self.action, name=name, kind='upgrade',
                    fields=dict(cmd='_s-xclick', encrypted=pp_order.encrypt(self.config)))
        
    def _buy_button(self, user_id, name, price):
        pp_order = payment.PaypalOrder(
                        custom=user_id,
                        item_name=name,
                        amount=price,
                        **self.common_buy)
        return dict(action=self.action, name=name,  kind='sms',
                    fields=dict(cmd='_s-xclick', encrypted=pp_order.encrypt(self.config)))        