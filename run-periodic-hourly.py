#!/usr/bin/env python
import sys
import logging
import os
from optparse import OptionParser
from ConfigParser import ConfigParser
from datetime import datetime, timedelta, date
from sqlalchemy import *
from sqlalchemy.orm import create_session
import pytz
from dateutil.relativedelta import relativedelta
from monimi import db
from monimi.db import User, Monitor, Facet, t_monitor, t_user, t_monitor_log, t_monitor_log_hist
from monimi.stats import HIST_AGG_MAP
from monimi.util import datetime_to_ts, ts_to_datetime, get_utc_offset

log = logging.getLogger(__name__)

def get_target_offsets(current_utc_hour, target_hour):
    return (target_hour - current_utc_hour) * 60, (24 + target_hour - current_utc_hour) * 60

def get_utc_day_times(target_tz, target_datetime):
        utc_start_of_day = target_tz.localize(datetime(target_datetime.year,
                                                       target_datetime.month,
                                                       target_datetime.day, 0, 0)).astimezone(pytz.UTC)
                                                       
        utc_end_of_day = target_tz.localize(datetime(target_datetime.year,
                                                     target_datetime.month,
                                                     target_datetime.day, 23, 59)).astimezone(pytz.UTC)
                                                     
        return datetime_to_ts(utc_start_of_day), datetime_to_ts(utc_end_of_day)

def hist_stats_for_monitor(session, monitor, target_datetime_ts, ts_day_start, ts_day_end):
        log.debug("Monitor %s", monitor.name)
        facets = session.query(Facet).filter_by(kind=monitor.kind).all()
        for facet in facets:
            for hfacet in facet.hist.values():
                result = HIST_AGG_MAP[hfacet.agg_type](session, monitor, facet, ts_day_start, ts_day_end)
                t_monitor_log_hist.insert().execute(monitor_id = monitor.monitor_id,
                                                        hist_facet_id = hfacet.hist_facet_id,
                                                        node = 0,
                                                        ts = target_datetime_ts,
                                                        value = result)
        # delete entries older than 14 days from monitor_log and ping_log 
        q = t_monitor_log.delete(and_(t_monitor_log.c.ts < datetime_to_ts(datetime.utcnow().date() - timedelta(days=14)),
                                      t_monitor_log.c.monitor_id == monitor.monitor_id))
        session.execute(q)
                
def hist_stats_for_user(session, user_id, days_back=1):
    user = session.query(User).get(user_id)
    log.debug("Doing stats for user %s", user.email)
    target_tz = pytz.timezone(user.timezone)
    target_datetime = datetime.now(tz=target_tz) - timedelta(days=days_back) 
    target_datetime_ts = datetime_to_ts(datetime(target_datetime.year, target_datetime.month, target_datetime.day, 0, 0))
    ts_day_start, ts_day_end = get_utc_day_times(target_tz, target_datetime)
    for monitor in user.monitors.values():
        hist_stats_for_monitor(session, monitor, target_datetime_ts, ts_day_start, ts_day_end)
    # update UTC offset
    user.utc_offset = get_utc_offset(pytz.timezone(user.timezone))
    session.flush()

def hist_stats(session):
    utcnow = datetime.now(tz=pytz.UTC)
    offset1, offset2 = get_target_offsets(utcnow.hour, target_hour=1)
    
    q = select([t_user.c.user_id],
                    or_(and_(t_user.c.utc_offset >= offset1, t_user.c.utc_offset < offset1 + 59),
                    and_(t_user.c.utc_offset >= offset2, t_user.c.utc_offset < offset2 + 59)))
    
    for (user_id,) in q.execute():
        hist_stats_for_user(session, user_id)


if __name__== "__main__":
    parser = OptionParser()
    parser.add_option("-b", "--back", dest="days_back", help="run as if today was DAYS_BACK days ago", 
                      metavar="DAYS_BACK", type="int", default=0)
    (options, args) = parser.parse_args()
     
    conf = ConfigParser()
    conf.read('%s/etc/config' % os.environ.get('MONIMI_HOME', '.'))


    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(levelname)s %(name)s %(message)s',
                        filename=os.path.join(os.environ.get('MONIMI_HOME', '.'), 'log', 'hourly.log'))

    db.engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    db.metadata.bind = db.engine
    connection = db.engine.connect()
    session = create_session(bind=connection)
    
    #today = (datetime.utcnow().date() - timedelta(days=options.days_back))
    log.info("Doing stats")
    hist_stats(session)
    log.info("Completed")
    
