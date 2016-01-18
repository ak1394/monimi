import logging
import random
import re
from datetime import datetime
from dateutil.relativedelta import relativedelta
import recaptcha.client.captcha as captcha
import simplejson
import pytz
from sqlalchemy import *
from sqlalchemy.sql import func

from monimi.db import User, ConfirmationCode, Destination, t_user
from monimi.web import jrpc, ValidationFailedException
from monimi import util
from monimi.geolocation import GeolocationService
from monimi import limits
from monimi import validators

log = logging.getLogger(__name__)

sms_status = {  1: "Message unknown or reporting is delayed",
                2: "Message queued",
                3: "Delivered to gateway",
                4: "Received by recipient",
                5: "Error with message",
                6: "User cancelled message delivery",
                7: "Error delivering message",
                8: "OK, Message received by gateway",
                9: "Routing error",
                10: "Message expired",
                11: "Message queued for later delivery",
                12: "Out of credit",              
              }

class RegistrationController(object):
    def __init__(self, captcha_key, geoip_city_filename):
        self.captcha_key = captcha_key
        self.geoservice = GeolocationService(geoip_city_filename)
        self.non_digits_regex = re.compile(r'\D')
            
    @jrpc()
    def get_location(self, r):
        return self.geoservice.get_record(r.environ['REMOTE_ADDR'])

    @jrpc()
    def sms_status(self, r, sms_id):
        status, data = r.hub.smsStatus(sms_id)
        if str(status) == 'ok':
            return dict(status_code=data, status_msg=sms_status[data])
        else:
            return dict(status_code=None, status_msg="Error: %s" % data)
    
    @jrpc(validator=validators.RegistrationSchema())
    def create_user(self, r, email, password, confirm, mobile, captcha_challenge, captcha_response):
        remote_ip = r.environ['REMOTE_ADDR']
        captcha_result = captcha.submit(captcha_challenge, captcha_response, self.captcha_key, remote_ip)
        if captcha_result.is_valid:
            # check for existing email and mobile
            self._check_duplicate_email(r.connection, email)
            self._check_duplicate_mobile(r.connection, mobile)

            log.debug('captcha is ok, saving registration data')
            confirm = ConfirmationCode()
            confirm.code = self._confirmation_code()
            mobile = ''.join(self.non_digits_regex.split(mobile))
            confirm.data = self._serialize(dict(email=email.lower(),
                                                password=password, 
                                                mobile=mobile))
            confirm.created = datetime.utcnow()
            status, data = r.hub.sendConfirmation(mobile, confirm.code)
            if str(status) == 'ok':
                r.session.save(confirm)
                r.session.flush()
                return dict(success=True, sms_id=data)
            else:
                log.debug('Failed to send sms: %s', data)
                raise ValidationFailedException(dict(mobile='Unable to send SMS: %s' % data))
        else:
            log.debug('captcha is invalid')
            raise ValidationFailedException(dict(captcha_response='Validation failed'))

    @jrpc(validator=validators.ConfirmationSchema())
    def confirm_user(self, r, confirmation_code, timezone):
        confirm = r.session.query(ConfirmationCode).filter_by(code=confirmation_code.lower()).first()
        if confirm:
            ud = self._deserialize(confirm.data)

            user = User(ud['email'], ud['mobile'])
            user.password = util.encrypt_password(ud['password'])
            user.created = datetime.utcnow()
            user.plan = 'free' 
            user.sms_credits_plan = limits.get_plan_credits('free')
            user.sms_credits_purchased = 0
            user.billing_type = 0
            user.timezone = timezone
            user.utc_offset = util.get_utc_offset(pytz.timezone(timezone))
            user.billing_start = datetime.utcnow()
            user.billing_next = user.billing_start + relativedelta(months=+1)
            
            # create two default destinations
            default_sms_destination = Destination(user, Destination.TYPE_SMS, 'Default SMS', user.mobile)
            default_email_destination = Destination(user, Destination.TYPE_EMAIL, 'Default Email', user.email)
            r.session.delete(confirm)
            r.session.save(user)
            r.session.save(default_sms_destination)
            r.session.save(default_email_destination)
            r.session.flush()
            r.hub.sendWelcome(user.email, user.password)
            r.environ['paste.auth_tkt.set_user'](userid=user.email)
        else:
            raise ValidationFailedException(dict(confirmation_code='Confirmation code is invalid'))

    def _confirmation_code(self):
        return util.base(random.getrandbits(40), 32).lower()
    
    def _serialize(self, data):
        return simplejson.dumps(data)
    
    def _deserialize(self, serialized):
        return simplejson.loads(serialized)

    def _check_duplicate_email(self, connection, new_email):
        user_count = connection.execute(select([func.count(t_user.c.user_id)],
                                                          func.lower(t_user.c.email) == new_email.lower())).fetchone()[0]
        if user_count:
            raise ValidationFailedException(dict(email='Account with this email already exists.'))

    def _check_duplicate_mobile(self, connection, new_mobile):
        user_count = connection.execute(select([func.count(t_user.c.user_id)],
                                                          t_user.c.mobile == new_mobile)).fetchone()[0]
        if user_count:
            raise ValidationFailedException(dict(mobile='Account with this mobile number already exists.'))
    
