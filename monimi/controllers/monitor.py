import logging
from datetime import datetime, timedelta
from sqlalchemy import *
import pytz
from pytz import utc
from monimi.web import jrpc, authorize, ValidationFailedException
from monimi import validators
from monimi.db import Monitor, AlertDestination, Destination, AlertDestinationNotifyOn, Facet
from monimi.db import t_monitor, t_monitor_log, t_error_log, t_monitor_log_hist
import monimi.db as db
import monimi.limits as limits
import monimi.permissions as perm
from monimi.util import encode_enum, decode_enum, utcdatetimefromiso, localtimefromtime, datetime_to_ts, ts_to_datetime
import simplejson

log = logging.getLogger(__name__)

class MonitorController(object):
    @jrpc()
    @authorize(perm.ValidUser())
    def list(self, r):
        result = []
        for monitor in sorted(r.user.monitors.values(), key=lambda m: m.name.lower()):
            r = dict(name=monitor.name, 
                 url=monitor.config,
                 state=encode_enum(Monitor, monitor.state),
                 kind=encode_enum(Monitor, monitor.kind, name='kind'),
                 user_state=encode_enum(Monitor, monitor.user_state, name='user_state'))
            result.append(r)
        return result

    @jrpc()
    @authorize(perm.ValidUser())
    def limits(self, r):
        max_monitors = limits.max_monitor_count(r.user)
        min_beat = limits.min_beat(r.user)
        sms_credits = r.user.sms_credits_plan + r.user.sms_credits_purchased
        allow_https = limits.is_https_allowed(r.user)
        return dict(max_monitors=max_monitors, 
                    min_beat=min_beat, 
                    sms_credits=sms_credits, 
                    sms_credis_warn=3,
                    allow_https=allow_https)

    @jrpc()
    @authorize(perm.ValidUser())
    def list_settings(self, r, name):
        monitor = r.user.monitors[name]
        return dict(beat=monitor.beat)

    @jrpc(validator=validators.MonitorSettingsSchema())
    @authorize(perm.ValidUser())
    def set_settings(self, r, name, beat, new_name):
        if new_name != name and new_name in r.user.monitors.keys():
            raise ValidationFailedException(dict(new_name='Check with name "%s" already exist' % new_name))

        if beat >= limits.min_beat(r.user):
            permitted_beat = beat
        else:
            permitted_beat = limits.min_beat(r.user)    
        monitor = r.user.monitors[name]
        monitor.beat = permitted_beat
        
        if new_name != name:
            monitor.name = new_name
            
        r.session.flush()
        r.hub.touchMonitor(monitor.monitor_id)
    
    @jrpc(validator=validators.MonitorNameSchema())
    @authorize(perm.ValidUser())
    def list_notifications(self, r, name):
        monitor = r.user.monitors[name]
        target_tz = pytz.timezone(r.user.timezone)
        result = []
        for ad in monitor.alert_destinations:
            result.append(
                dict(name=ad.destination.destination_name,
                     destination_type=encode_enum(Destination, ad.destination.destination_type),
                     notify_down_on=encode_enum(AlertDestinationNotifyOn, ad.notify_down_on),
                     notify_up=ad.notify_up,
                     qp_start=localtimefromtime(ad.qp_start, target_tz),
                     qp_end=localtimefromtime(ad.qp_end, target_tz),
                )
            )
        return result

    @jrpc()
    @authorize(perm.ValidUser())
    def set_notifications(self, r, name, notifications):
        target_tz = pytz.timezone(r.user.timezone)
        monitor = r.user.monitors[name]
        for notification in notifications:
            destination = r.user.destinations[notification['name']]
            ad = r.session.query(AlertDestination).filter_by(
                            destination_id=destination.destination_id,
                            monitor_id=monitor.monitor_id).one()
            ad.notify_down_on = decode_enum(AlertDestinationNotifyOn, notification['notify_down_on'])
            ad.notify_up = notification['notify_up']
            if notification['qp_start']:
                ad.qp_start = utcdatetimefromiso(notification['qp_start'], target_tz).time()
            else:
                ad.qp_start = None
            if notification['qp_end']:
                ad.qp_end = utcdatetimefromiso(notification['qp_end'], target_tz).time()
            else:
                ad.qp_end = None
            r.session.flush()
        r.hub.touchMonitor(monitor.monitor_id)
        return True

    @jrpc(validator=validators.MonitorAddSchema())
    @authorize(perm.ValidUser(), perm.AddMonitor())
    def add_http(self, r, name, url, beat):
        monitor = Monitor(url, Monitor.KIND_HTTP, name)
        self._add_monitor(r, monitor, beat)

    @jrpc(validator=validators.MonitorAddSchema())
    @authorize(perm.ValidUser(), perm.AddMonitor(), perm.HttpsAllowed())
    def add_https(self, r, name, url, beat):
        monitor = Monitor(url, Monitor.KIND_HTTPS, name)
        self._add_monitor(r, monitor, beat)

    @jrpc(validator=validators.MonitorAddPingSchema())
    @authorize(perm.ValidUser(), perm.AddMonitor())
    def add_ping(self, r, name, host, beat):
        monitor = Monitor(host, Monitor.KIND_PING, name)
        self._add_monitor(r, monitor, beat)

    @jrpc(validator=validators.MonitorAddDnsSchema())
    @authorize(perm.ValidUser(), perm.AddMonitor())
    def add_dns(self, r, name, hostname, beat, ns, qtype):
        config = [{'name': hostname.lower()}]
        if ns and ns != 'any-ns':
            config.append({'ns':ns.lower()})
        if qtype:
            config.append({'qtype':qtype.lower()})
        config = simplejson.dumps(config)
        monitor = Monitor(config, Monitor.KIND_DNS, name)
        self._add_monitor(r, monitor, beat)

    @jrpc()
    def get_nameservers(self, r, name):
        status, data = r.hub.getNameservers(name)
        if str(status) == 'ok':
            domain, nameservers = data
            nameservers.sort()
            return dict(domain=domain, nameservers=nameservers)
        else:
            return dict(error="Unable to lookup nameservers: %s" % data)

    def _add_monitor(self, r, monitor, beat):
        if monitor.name in r.user.monitors.keys():
            raise ValidationFailedException(dict(name='Check with name "%s" already exist' % monitor.name))
        
        if monitor.config in [m.config for m in r.user.monitors.values()]:
            raise ValidationFailedException(dict(url='You already have a check for this target'))

        monitor.user_id = r.user.user_id
        monitor.state = Monitor.ST_ADDED
        monitor.user_state = Monitor.U_ST_NORMAL
        monitor.beat = beat if beat >= limits.min_beat(r.user) else limits.min_beat(r.user)
         
        default_email = r.session.query(Destination).filter_by(user=r.user, destination_name='Default Email').one()
        default_sms = r.session.query(Destination).filter_by(user=r.user, destination_name='Default SMS').one()
        
        umd_email = AlertDestination(monitor, default_email)
        umd_email.alert_destination_state = AlertDestination.ST_NEW
        umd_email.notify_down_on = 1
        umd_email.notify_up = 0
        
        umd_sms = AlertDestination(monitor, default_sms)
        umd_sms.alert_destination_state  = AlertDestination.ST_NEW
        umd_sms.notify_down_on = 1
        umd_sms.notify_up = 0

        r.session.save(monitor)
        r.session.save(umd_email)
        r.session.save(umd_sms)
        r.session.flush()
        
        r.hub.touchMonitor(monitor.monitor_id)

    @jrpc(validator=validators.MonitorNameSchema())
    @authorize(perm.ValidUser())
    def pause(self, r, name):
        monitor = r.user.monitors[name]
        monitor.user_state = Monitor.U_ST_PAUSED
        r.hub.touchMonitor(monitor.monitor_id)
        r.session.flush()

    @jrpc(validator=validators.MonitorNameSchema())
    @authorize(perm.ValidUser())
    def resume(self, r, name):
        monitor = r.user.monitors[name]
        monitor.user_state = Monitor.U_ST_NORMAL
        r.hub.touchMonitor(monitor.monitor_id)
        r.session.flush()

    @jrpc(validator=validators.MonitorNameSchema())
    @authorize(perm.ValidUser())
    def remove(self, r, name):
        monitor = r.user.monitors[name]
        # remove data from appropriate tables by facet
        r.connection.execute(t_monitor_log.delete(t_monitor_log.c.monitor_id == monitor.monitor_id))
        r.connection.execute(t_error_log.delete(t_error_log.c.monitor_id == monitor.monitor_id))
        r.connection.execute(t_monitor_log_hist.delete(t_monitor_log_hist.c.monitor_id == monitor.monitor_id))
        for alert_destination in monitor.alert_destinations:
            r.session.delete(alert_destination)
        r.session.delete(monitor) 
        r.session.flush() 
        r.hub.touchMonitor(monitor.monitor_id)

    @jrpc(validator=validators.MonitorNameSchema())
    @authorize(perm.ValidUser())
    def daily_stats(self, r, name):
        target_tz = pytz.timezone(r.user.timezone)
        now = datetime.utcnow()
        monitor = r.user.monitors[name]
        facet_name_by_kind = {
                         Monitor.KIND_HTTP: 'time',
                         Monitor.KIND_HTTPS: 'time',
                         Monitor.KIND_PING: 'rtt_avg',
                         Monitor.KIND_DNS: 'time',
                         }
        
        facet_name = facet_name_by_kind[monitor.kind]
        facet = r.session.query(Facet).filter_by(kind=monitor.kind, name=facet_name).one()

        query = select([t_monitor_log.c.ts, t_monitor_log.c.node, t_monitor_log.c.value],
                       and_(t_monitor_log.c.facet_id == facet.facet_id,
                            t_monitor_log.c.monitor_id == monitor.monitor_id,
                            t_monitor_log.c.ts > datetime_to_ts(now - timedelta(days=1))))
        query = query.order_by(t_monitor_log.c.ts.desc())
        
        if monitor.kind in [Monitor.KIND_HTTP, Monitor.KIND_HTTPS]:
            title = "Response time and availability for last 24 hours"
        elif monitor.kind == Monitor.KIND_PING:
            title = "Round trip time and availability for last 24 hours"
        else:
            title = "Unknown"

        all_series = [dict(title=title), 
                      [dict(legend='Missouri, US')],
                      [dict(legend='Georgia, US')],
                      [dict(legend='London, UK')],
                      ]
        
        result = r.session.execute(query)
        for ts, node, value in result:
            all_series[node].append(datetime_to_ts(ts_to_datetime(ts).astimezone(target_tz)))
            all_series[node].append(value)

        return all_series
