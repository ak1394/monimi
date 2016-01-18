import logging
from datetime import datetime, date, time, timedelta
import calendar
from sqlalchemy import *
import pytz
from pytz import utc
from monimi.web import jrpc, authorize
from monimi.db import Monitor, Facet, HistFacet, t_monitor_log, t_monitor_log_hist,  t_error_log
import monimi.permissions as perm
from monimi import validators
from monimi.util import datetime_to_ts, ts_to_datetime, utcdatetimefromiso, datefromiso

log = logging.getLogger(__name__)

class ReportList(object):
    def __init__(self, session):
        self.session = session
        
    def date_range(self, start, end):
        r = (end + timedelta(days=1) - start).days
        return [start + timedelta(days=i) for i in range(r)]
            
    def daily(self, monitor, target_tz):
        # select earliest and latest ts for main monitor facet
        query = select([func.min(t_monitor_log.c.ts), func.max(t_monitor_log.c.ts)],
                       t_monitor_log.c.monitor_id == monitor.monitor_id)
        start, end = self.session.execute(query).fetchall()[0]
    
        if start and end:
            start = ts_to_datetime(start).astimezone(target_tz)
            end = ts_to_datetime(end).astimezone(target_tz)
            return [day for day in  self.date_range(start.date(), end.date())]
        return []


    def monthly(self, monitor, target_tz):
        monthly_results = []
        query = select([func.min(t_monitor_log_hist.c.ts)],
                       t_monitor_log_hist.c.monitor_id == monitor.monitor_id)
        start = self.session.execute(query).fetchall()[0][0]
        this_month = datetime.now(tz=utc).astimezone(target_tz).date().replace(day=1)
        if start:
            start = ts_to_datetime(start).date()
            # months starting from earliest date in monitor_log_daily
            # but not current month
            current = start.replace(day=1)
            while current < this_month:
                monthly_results.append(current)
                if current.month == 12:
                    current = current.replace(year=current.year+1, month=1)
                else:
                    current = current.replace(month=current.month+1)
        # always add the current month
        monthly_results.append(this_month)
        return monthly_results

    def yearly(self, monitor):
        return []

class DailyReport(object):
    def __init__(self, session, report_plan):
        self.session = session
        self.report_plan = report_plan

    def generate(self, monitor, target_tz, date_for):
        start = utcdatetimefromiso(date_for, target_tz)
        end = start + timedelta(days=1)
        facet_names = [item['facet'] for item in self.report_plan if item.has_key('facet')]

        facets = self.session.query(Facet).filter(Facet.kind==monitor.kind).filter(Facet.name.in_(facet_names)).all()

        facet_ids = []
        facet_by_id = dict()
        for facet in facets:
            facet_ids.append(facet.facet_id)
            facet_by_id[facet.facet_id] = facet
            

        pre_result = dict()
        
        # get facets
        query = select([t_monitor_log.c.facet_id, t_monitor_log.c.ts, t_monitor_log.c.node, t_monitor_log.c.value],  
                       and_(t_monitor_log.c.monitor_id==monitor.monitor_id,
                            t_monitor_log.c.facet_id.in_(facet_ids),
                            t_monitor_log.c.ts >= datetime_to_ts(start),
                            t_monitor_log.c.ts < datetime_to_ts(end)))
        for facet_id, ts, node, value in self.session.execute(query):
            facet = facet_by_id[facet_id]
            pre_result.setdefault(ts, dict())[facet.name] = value
            if node is not None:
                pre_result.setdefault(ts, dict())['node'] = node

        # get errors
        query = select([t_error_log.c.ts, t_error_log.c.node, t_error_log.c.error],  
                       and_(t_error_log.c.monitor_id==monitor.monitor_id,
                            t_error_log.c.ts >= datetime_to_ts(start),
                            t_error_log.c.ts < datetime_to_ts(end)))

        for ts, node, err in self.session.execute(query):
            pre_result.setdefault(ts, dict())['error'] = err
        
        result = []
        sorted_ts = pre_result.keys()
        sorted_ts.sort()
        for ts in sorted_ts:
            data = pre_result[ts]
            data['ts'] = long(datetime_to_ts(ts_to_datetime(ts).astimezone(target_tz)))
            result.append(data)

        return [self.report_plan, result]

class MonthlyReport(object):
    def __init__(self, session, report_plan):
        self.session = session
        self.report_plan = report_plan

    def generate(self, monitor, target_tz, date_for):
        start = datefromiso(date_for).replace(day=1)
        today = datetime.now(tz=utc).astimezone(target_tz).date()
        
        # depending whether the report is for the current month or one 
        # of the past months, the end date for report might be 
        # the last day of some (past) month, or today
        if today.year == start.year and today.month == start.month:
            end = today
        else:
            _, last_day = calendar.monthrange(start.year, start.month)
            end = date(start.year, start.month, last_day)
        
        facet_names = [item['facet'] for item in self.report_plan if item.has_key('facet')]

        facets = self.session.query(Facet).filter(Facet.kind==monitor.kind).all()
        hist_facets = []
        for facet in facets:
            for hist_facet_name, hist_facet in facet.hist.items():
                if hist_facet_name in facet_names:
                    hist_facets.append(hist_facet)
                
        hist_facet_ids = []
        hist_facet_by_id = dict()
        for hist_facet in hist_facets:
            hist_facet_ids.append(hist_facet.hist_facet_id)
            hist_facet_by_id[hist_facet.hist_facet_id] = hist_facet

        pre_result = dict()
        # get facets
        query = select([t_monitor_log_hist.c.hist_facet_id, t_monitor_log_hist.c.ts, t_monitor_log_hist.c.node, t_monitor_log_hist.c.value],  
                       and_(t_monitor_log_hist.c.monitor_id==monitor.monitor_id,
                            t_monitor_log_hist.c.hist_facet_id.in_(hist_facet_ids),
                            t_monitor_log_hist.c.ts >= datetime_to_ts(start),
                            t_monitor_log_hist.c.ts <= datetime_to_ts(end)))
        
        for hist_facet_id, ts, node, value in self.session.execute(query):
            hist_facet = hist_facet_by_id[hist_facet_id]
            pre_result.setdefault(ts, dict())[hist_facet.name] = value
            if node is not None:
                pre_result.setdefault(ts, dict())['node'] = node

        
        result = []
        sorted_ts = pre_result.keys()
        sorted_ts.sort()
        for ts in sorted_ts:
            data = pre_result[ts]
            data['ts'] = ts
            result.append(data)
        return [self.report_plan, result]

class ReportController(object):
    @jrpc(validator=validators.ReportNameSchema())
    @authorize(perm.ValidUser())
    def list(self, r, monitor):
        monitor = r.user.monitors[monitor]
        target_tz = pytz.timezone(r.user.timezone)
        rl = ReportList(r.session)
        daily_results, monthly_results, yearly_results = (rl.daily(monitor, target_tz),
                                                         rl.monthly(monitor, target_tz),
                                                         rl.yearly(monitor)) 
        return dict(daily=daily_results, monthly=monthly_results, yearly=yearly_results)

    @jrpc(validator=validators.ReportSchema())
    @authorize(perm.ValidUser())
    def daily(self, r, monitor, date_for):
        monitor = r.user.monitors[monitor]
        target_tz = pytz.timezone(r.user.timezone)
        if monitor.kind in [Monitor.KIND_HTTP, Monitor.KIND_HTTPS]:
            report_plan = [dict(title="Date / Time", name="ts", type="timestamp", format="timestamp"),
                     dict(title="Status", type="error", format="error"),
                     dict(title="Response Time", name="time", facet="time", type="normal", format="normal", unit="ms"),
                     dict(title="Location", name="node", type="node", format="node")]
        elif monitor.kind == Monitor.KIND_PING:
            report_plan = [dict(title="Date / Time", name="ts", type="timestamp", format="timestamp"),
                     dict(title="Status", type="error", format="error"),
                     dict(title="RTT", name="rtt_avg", facet="rtt_avg", type="normal", format="normal", unit="ms"),
                     dict(title="RTT Min", name="rtt_min", facet="rtt_min", type="normal", format="normal", unit="ms"),
                     dict(title="RTT Max", name="rtt_max", facet="rtt_max", type="normal", format="normal", unit="ms"),
                     dict(title="Packets Lost", name="packet_loss", facet="packet_loss", type="normal", format="normal", unit="%"),
                     dict(title="Location", name="node", type="node", format="node")]
        elif monitor.kind == Monitor.KIND_DNS:
            report_plan = [dict(title="Date / Time", name="ts", type="timestamp", format="timestamp"),
                     dict(title="Status", type="error", format="error"),
                     dict(title="Response Time", name="time", facet="time", type="normal", format="normal", unit="ms"),
                     dict(title="Location", name="node", type="node", format="node")]
        report = DailyReport(r.session, report_plan)
        result = report.generate(monitor, target_tz, date_for)
        return result
    
    @jrpc(validator=validators.ReportSchema())
    @authorize(perm.ValidUser())
    def monthly(self, r, monitor, date_for):
        monitor = r.user.monitors[monitor]
        target_tz = pytz.timezone(r.user.timezone)
        if monitor.kind in [Monitor.KIND_HTTP, Monitor.KIND_HTTPS]:
            report_plan = [dict(title="Date", name="ts", type="timestamp", format="timestamp", selector="date"),
                     dict(title="Checks", name="checks", facet="checks", type='normal', format="normal"),
                     dict(title="Downtime", name="downtime", facet="downtime", type='normal', format="duration"),
                     dict(title="Time Min", name="time_min", facet="time_min", type="normal", format="normal", unit="ms"),
                     dict(title="Time Max", name="time_max", facet="time_max", type="normal", format="normal", unit="ms"),
                     dict(title="Time Avg", name="time_avg", facet="time_avg", type="normal", format="normal", unit="ms")]
        elif monitor.kind == Monitor.KIND_PING:
            report_plan = [dict(title="Date", name="ts", type="timestamp", format="timestamp", selector="date"),
                     dict(title="Checks", name="checks", facet="checks", type='normal', format="normal"),
                     dict(title="Downtime", name="downtime", facet="downtime", type='normal', format="duration"),
                     dict(title="RTT Min", name="rtt_min", facet="rtt_min", type="normal", format="normal", unit="ms"),
                     dict(title="RTT Max", name="rtt_max", facet="rtt_max", type="normal", format="normal", unit="ms"),
                     dict(title="RTT Avg", name="rtt_avg", facet="rtt_avg", type="normal", format="normal", unit="ms"),
                     dict(title="Packet Loss Average", name="packet_loss_avg", facet="packet_loss_avg", type="normal", format="normal", unit="%"),
                     ]
        elif monitor.kind == Monitor.KIND_DNS:
            report_plan = [dict(title="Date", name="ts", type="timestamp", format="timestamp", selector="date"),
                     dict(title="Checks", name="checks", facet="checks", type='normal', format="normal"),
                     dict(title="Downtime", name="downtime", facet="downtime", type='normal', format="duration"),
                     dict(title="Time Min", name="time_min", facet="time_min", type="normal", format="normal", unit="ms"),
                     dict(title="Time Max", name="time_max", facet="time_max", type="normal", format="normal", unit="ms"),
                     dict(title="Time Avg", name="time_avg", facet="time_avg", type="normal", format="normal", unit="ms")]
        report = MonthlyReport(r.session, report_plan)
        result = report.generate(monitor, target_tz, date_for)
        return result
