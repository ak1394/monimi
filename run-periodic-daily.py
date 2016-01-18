#!/usr/bin/env python
import sys
import logging
import os
from ConfigParser import ConfigParser
from datetime import datetime, timedelta, date
from sqlalchemy import *
from sqlalchemy.orm import create_session
import pytz
from dateutil.relativedelta import relativedelta
from monimi import db
from monimi.db import t_user
from monimi.limits import get_plan_credits

log = logging.getLogger(__name__)

def daily_billing(session):
    q = select([t_user.c.user_id, t_user.c.billing_next, t_user.c.plan], t_user.c.billing_type == 0)
    today = date.today()
    for user_id, billing_next, plan in q.execute():
        if billing_next == today:
            new_credits = get_plan_credits(plan)
            new_billing_next = billing_next + relativedelta(months=+1)
            log.info("User %d doing billing cycle: new credits %s, next billing date: %s", user_id, new_credits, new_billing_next)
            u = t_user.update().where(t_user.c.user_id==user_id).values(billing_next=new_billing_next,
                                                                        sms_credits_plan=new_credits)
            session.execute(u)

if __name__== "__main__":
    conf = ConfigParser()
    conf.read('%s/etc/config' % os.environ.get('MONIMI_HOME', '.'))


    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(levelname)s %(name)s %(message)s',
                        filename=os.path.join(os.environ.get('MONIMI_HOME', '.'), 'log', 'daily.log'))

    db.engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    db.metadata.bind = db.engine
    connection = db.engine.connect()
    session = create_session(bind=connection)
    
    log.info("Doing billing")
    daily_billing(session)
    log.info("Completed")
    