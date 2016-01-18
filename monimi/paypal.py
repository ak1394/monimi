import logging
import random
from traceback import format_exc

from sqlalchemy.orm import create_session
from genshi.template import TemplateLoader

import monimi
from monimi.db import User
from monimi import payment

log = logging.getLogger(__name__)

loader = TemplateLoader('templates', auto_reload=True)
payment_completed_tmpl = loader.load('payment_completed.html')
payment_failed_tmpl = loader.load('payment_failed.html')
subscription_modified_tmpl = loader.load('subscription_modified.html')

config = None

def web(f):
    def web_call(r):
        log.debug("invoking %s", f)
        connection = monimi.db.engine.connect()
        session = create_session(bind=connection)
        try:
            transaction = connection.begin()
            result = f(r, config, session)
            transaction.commit()
            return result
        except:
            log.error("exception: %s, issuing rollback", format_exc())
            transaction.rollback()
            r.res.status = "500 Internal Server Error"
            return "Internal Server Error"
        finally:
            session.close()
            connection.close()
            log.debug("completed")
    return web_call    

@web
def pdt(r, config, session):
    params = r.query
    rand_id = random.randint(10000, 100000)
    log.debug("PDT received: %d, %s", rand_id, params)
    if params.has_key('tx'):
        result = payment.verify_pdt(params['tx'], session, config)
        log.debug("result: %s", result)
        if result:
            return payment_completed_tmpl.generate(price=result['payment_gross'],
                                                   currency=result['mc_currency'],
                                                   name=result['item_name']).render('html', doctype='html')
        else:
            log.error('Invalid PDT %d, failed to verify', rand_id)
    else:
        log.error('Invalid PDT %d, no tx', rand_id)
    return payment_failed_tmpl.generate(transaction_id=rand_id).render('html', doctype='html')
    
@web
def ipn(r, config, session):
    params = r.form
    log.debug("IPN received: %s", params)
    if not payment.verify_ipn(params, config):
        log.error('IPN is invalid')
        raise Exception('Invalid IPN')
    if params.has_key('txn_type') and _IPN_DISPATCH.has_key(params['txn_type']):
        f = _IPN_DISPATCH[params['txn_type']]
        log.debug('Dispatching to: %s', f)
        f(params, config, session)
        session.flush()
    else:
        log.error('Unknown IPN')
    return "OK"

def ipn_subscription(params, config, session):
    log.info('New Subscription')

def ipn_subscription_payment(params, config, session):
    log.info('Subscription Payment')
    if not payment.is_unique_transaction(params['txn_id'], session):
        log.error('Duplicate transaction: %s', params['txn_id'])
        raise Exception('Duplicate Transaction')
    if params.has_key('custom'):
        log.debug('New subscription payment')
        _ipn_new_subscription_payment(params, config, session)
    elif params.has_key('subscr_id'):
        log.debug('Recurring subscription payment')
        _ipn_recurring_subscription_payment(params, config, session)
    else:
        log.error('Unable to process transaction, no custom or subscr_id parameter')
        raise Exception('Unable to process transaction')

def _ipn_new_subscription_payment(params, config, session):
    user = session.query(User).get(int(params['custom']))
    assert user is not None
    payment.record_transaction(user.user_id, params['txn_id'], params, session)
    user.subscr_id = params['subscr_id']
    # assign plan and sms credits
    user.plan = _plan_by_name(params['item_name'])
    user.sms_credits_plan = _credits_by_plan(user.plan)
    
def _ipn_recurring_subscription_payment(params, config, session):
    user = session.query(User).filter_by(subscr_id=params['subscr_id']).first()
    assert user is not None
    payment.record_transaction(user.user_id, params['txn_id'], params, session)
    # TODO check if the user.plan matches params['item_name']
    user.sms_credits_plan = _credits_by_plan(user.plan)

def ipn_subscription_change(params, config, session):
    # TODO
    pass

def ipn_subcription_cancel(params, config, session):
    user = session.query(User).filter_by(subscr_id=params['subscr_id']).first()
    assert user is not None
    log.info('Subscription canceled, user id: %s', user.user_id)
    user.plan = 'free'
    user.sms_credits_plan = 0
    user.subscr_id = None

def ipn_purchase(params, config, session):
    log.info('Purchase')
    if not payment.is_unique_transaction(params['txn_id'], session):
        log.error('Duplicate transaction: %s', params['txn_id'])
        raise Exception('Duplicate Transaction')
    user = session.query(User).get(int(params['custom']))
    assert user is not None
    payment.record_transaction(user.user_id, params['txn_id'], params, session)
    new_credits_purchased = _credits_by_item_name(params['item_name'])
    log.info('User %d purchased %d SMS credits.', user.user_id, new_credits_purchased)
    user.sms_credits_purchased = user.sms_credits_purchased + new_credits_purchased

def _plan_by_name(plan_name):
    return _PLAN_BY_NAME[plan_name]

def _credits_by_plan(plan):
    return _SMS_CRDITS_BY_PLAN[plan]

def _credits_by_item_name(item_name):
    return _SMS_CRDITS_BY_ITEM_NAME[item_name]
        
_IPN_DISPATCH = dict(subscr_signup=ipn_subscription,
                subscr_payment=ipn_subscription_payment,
                subscr_modify=ipn_subscription_change,
                subscr_cancel=ipn_subcription_cancel,
                web_accept=ipn_purchase,
                )

_PLAN_BY_NAME = {'Basic Package':'basic',
                 'Pro Package': 'pro',
                 'Premium Package': 'premium',
                 }

_SMS_CRDITS_BY_PLAN = dict(basic=10, pro=20, premium=50)

_SMS_CRDITS_BY_ITEM_NAME = {'20 SMS Credits': 20,
                            '50 SMS Credits': 50,
                            } 
