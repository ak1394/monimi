from datetime import datetime, date, time, timedelta
import logging
import calendar
from functools import partial
from sqlalchemy import *
from pytz import utc
import pytz
from dateutil.relativedelta import relativedelta
from monimi.db import t_monitor_log
from monimi.util import utcdatetimefromiso, datefromiso
from monimi.util import datetime_to_ts, ts_to_datetime

log = logging.getLogger(__name__)

def hist_count(session, monitor, facet, ts_start, ts_end):
    q = select([func.count(t_monitor_log.c.ts)],
               and_(t_monitor_log.c.monitor_id == monitor.monitor_id,
                    t_monitor_log.c.facet_id == facet.facet_id,
                    t_monitor_log.c.ts >= ts_start,
                    t_monitor_log.c.ts < ts_end))
    result = session.execute(q).fetchall()[0][0]
    return result

def hist_downtime(session, monitor, facet, ts_start, ts_end):
    q = select([t_monitor_log.c.ts, t_monitor_log.c.value],
               and_(t_monitor_log.c.monitor_id == monitor.monitor_id,
                    t_monitor_log.c.facet_id == facet.facet_id,
                    t_monitor_log.c.ts >= ts_start,
                    t_monitor_log.c.ts < ts_end))
    q = q.order_by(t_monitor_log.c.ts.asc())

    samples = []
    for ts, value in session.execute(q).fetchall():
        samples.append((ts_to_datetime(ts), value))
    downtime = timedelta(0)
    for i in range(len(samples)-1): # disregard the last sample (TODO: really?)
        ts, value = samples[i]
        next_ts, next_value = samples[i+1]
        if value is None:
            downtime = downtime + next_ts - ts
    result = downtime.seconds
    return result 

def hist_avg(session, monitor, facet, ts_start, ts_end):
    q = select([func.avg(t_monitor_log.c.value)],
               and_(t_monitor_log.c.monitor_id == monitor.monitor_id,
                    t_monitor_log.c.facet_id == facet.facet_id,
                    t_monitor_log.c.ts >= ts_start,
                    t_monitor_log.c.ts < ts_end))
    result = session.execute(q).fetchall()[0][0]
    return result

def hist_min(session, monitor, facet, ts_start, ts_end):
    q = select([func.min(t_monitor_log.c.value)],
               and_(t_monitor_log.c.monitor_id == monitor.monitor_id,
                    t_monitor_log.c.facet_id == facet.facet_id,
                    t_monitor_log.c.ts >= ts_start,
                    t_monitor_log.c.ts < ts_end))
    result = session.execute(q).fetchall()[0][0]
    return result
                    
def hist_max(session, monitor, facet, ts_start, ts_end):
    q = select([func.max(t_monitor_log.c.value)],
               and_(t_monitor_log.c.monitor_id == monitor.monitor_id,
                    t_monitor_log.c.facet_id == facet.facet_id,
                    t_monitor_log.c.ts >= ts_start,
                    t_monitor_log.c.ts < ts_end))
    result = session.execute(q).fetchall()[0][0]
    return result

HIST_AGG_MAP = dict(count=hist_count,
                downtime=hist_downtime,
                avg=hist_avg,
                min=hist_min,
                max=hist_max)
