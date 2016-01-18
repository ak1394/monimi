from sqlalchemy import *
from sqlalchemy.orm import *
from sqlalchemy.orm.collections import column_mapped_collection, attribute_mapped_collection
from monimi.util import register_enum

metadata = MetaData()

t_user = Table('user', metadata,
    Column('user_id', Integer, primary_key=True),
    Column('email', String(128), unique=True, nullable=False),
    Column('username', String(48), unique=True, nullable=True),
    Column('password', String(128), nullable=False),
    Column('first_name', Unicode(64)),
    Column('last_name', Unicode(64)),
    Column('mobile', String(12),  unique=True),
    Column('created', DateTime),
    Column('access', DateTime),
    Column('login', DateTime),
    Column('timezone', String(32)),
    Column('utc_offset', Integer),
    Column('plan', String(8),  nullable=False), 
    Column('sms_credits_plan', Integer(unsigned=True), nullable=False), 
    Column('sms_credits_purchased', Integer(unsigned=True), nullable=False),
    Column('subscr_id', String(19), unique=True),
    Column('billing_type', Integer),
    Column('billing_start', Date),
    Column('billing_next', Date),
    mysql_engine='InnoDB'
)

t_confirmation = Table('confirmation', metadata,
    Column('code', String(16), primary_key=True),
    Column('data', String(1024)),
    Column('created', DateTime),
    mysql_engine='InnoDB'
)

t_destination = Table('destination', metadata,
    Column('destination_id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('user.user_id')),
    Column('destination_type', Integer, nullable=False),    
    Column('destination_name', String(128), nullable=False),    
    Column('address', String(128), nullable=False),    
    UniqueConstraint('user_id', 'destination_type', 'address'),
    UniqueConstraint('user_id', 'destination_name'),
    mysql_engine='InnoDB'
)

t_sms = Table('sms', metadata,
    Column('sms_id', Integer, primary_key=True),
    Column('user_id', Integer),
    Column('message', String(160), nullable=False),
    Column('phone', String(128), nullable=False),
    Column('reference', String(32)),
    Column('time_stamp', DateTime, nullable=False),
    Column('msg_id', String(128)),
    mysql_engine='InnoDB'
)

t_paypal_payments = Table('paypal_payments', metadata,
    Column('payment_id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('user.user_id'), nullable=False),
    Column('txn_id', String(19), unique=True, nullable=False),
    Column('data', Text, nullable=False),
    mysql_engine='InnoDB'
) 

t_monitor = Table('monitor', metadata,
    Column('monitor_id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('user.user_id'), nullable=False),
    Column('name', String(32), nullable=False),
    Column('config', String(512), nullable=False),
    Column('beat', Integer, nullable=False),    
    Column('state', Integer, nullable=False),
    Column('user_state', Integer, nullable=False),
    Column('kind', Integer, nullable=False),
    Column('parent', Integer),
    UniqueConstraint('user_id', 'config', 'kind'),
    UniqueConstraint('user_id', 'name'),
    mysql_engine='InnoDB'
)

t_facet = Table('facet', metadata,
    Column('facet_id', Integer, primary_key=True),
    Column('kind', Integer, nullable=False),
    Column('name', String(32)),
    Column('unit', String(16)),
    Column('info', String(256)),
    UniqueConstraint('kind', 'name'),
    mysql_engine='InnoDB'
)

t_hist_facet = Table('hist_facet', metadata,
    Column('hist_facet_id', Integer, primary_key=True),
    Column('facet_id', Integer, ForeignKey('facet.facet_id'), nullable=False),
    Column('name', String(32)),
    Column('agg_type', String(32)),
    Column('info', String(256)),
    UniqueConstraint('facet_id', 'name', 'agg_type'),
    mysql_engine='InnoDB'
)

t_monitor_log = Table('monitor_log', metadata,
    Column('monitor_id', Integer, ForeignKey('monitor.monitor_id'), nullable=False),
    Column('facet_id', Integer, ForeignKey('facet.facet_id'), nullable=False),
    Column('node', Integer),
    Column('ts', Integer, nullable=False),    
    Column('value', Integer),
    UniqueConstraint('monitor_id', 'facet_id', 'node', 'ts'),
    mysql_engine='InnoDB'
)

t_error_log = Table('error_log', metadata,
    Column('monitor_id', Integer, ForeignKey('monitor.monitor_id'), nullable=False),
    Column('node', Integer),
    Column('ts', Integer, nullable=False),    
    Column('error', String(128)),
    mysql_engine='InnoDB'
)

t_monitor_log_hist = Table('monitor_log_hist', metadata,
    Column('monitor_id', Integer, ForeignKey('monitor.monitor_id'), nullable=False),
    Column('hist_facet_id', Integer, ForeignKey('hist_facet.hist_facet_id'), nullable=False),
    Column('node', Integer),
    Column('ts', Integer, nullable=False),    
    Column('value', Integer),
    UniqueConstraint('monitor_id', 'hist_facet_id', 'node', 'ts'),
    mysql_engine='InnoDB'
)

Index('ix_monitor_log_1', t_monitor_log.c.monitor_id, 
                          t_monitor_log.c.facet_id,
                          t_monitor_log.c.ts)

Index('ix_monitor_log_hist_1', t_monitor_log_hist.c.monitor_id,
                               t_monitor_log_hist.c.hist_facet_id,
                               t_monitor_log_hist.c.ts)

t_alert_destination = Table('alert_destination', metadata,
    Column('alert_destination_id', Integer, primary_key=True),
    Column('monitor_id', Integer, ForeignKey('monitor.monitor_id'), nullable=False),
    Column('destination_id', Integer, ForeignKey('destination.destination_id'), nullable=False),
    Column('alert_destination_state', Integer, nullable=False),
    Column('notify_down_on', Integer, nullable=False),
    Column('notify_up', Boolean, nullable=False, default=False),    
    Column('qp_start', Time),    
    Column('qp_end', Time),
    UniqueConstraint('monitor_id', 'destination_id'),
    mysql_engine='InnoDB'
)

class User(object):
    def __init__(self, email, mobile):
        self.email = email
        self.mobile = mobile

    def show_ads(self):
        return self.plan == 'free'

class Destination(object):
    TYPE_SMS, TYPE_EMAIL = range(2)
    def __init__(self, user, destination_type, name, address):
        self.user = user
        self.destination_type =  destination_type
        self.destination_name = name
        self.address = address
register_enum(Destination, prefix='TYPE_')        

class ConfirmationCode(object):
    pass
        
class Monitor(object):
    ST_ADDED, ST_UP, ST_DOWN, ST_DISABLED, ST_ERROR = range(5)
    U_ST_NORMAL, U_ST_PAUSED = range(2)
    KIND_HTTP, KIND_HTTPS, KIND_PING, KIND_DNS  = range(4)
    def __init__(self, config, kind, name):
        self.config = config
        self.kind = kind
        self.name = name

register_enum(Monitor, prefix='ST_')        
register_enum(Monitor, prefix='KIND_', name='kind')        
register_enum(Monitor, prefix='U_ST_', name='user_state')        

class Facet(object):
    def __init__(self, kind, name, unit, info):
        self.kind = kind
        self.name = name
        self.unit = unit
        self.info = info

class HistFacet(object):
    def __init__(self, facet, name, agg_type, info):
        self.facet = facet
        self.name = name
        self.agg_type = agg_type
        self.info = info

class AlertDestination(object):
    ST_OK, ST_NOTIFIED_DOWN, ST_NEW = range(3)
    def __init__(self, monitor, destination):
        self.monitor = monitor
        self.destination = destination
register_enum(AlertDestination, prefix='ST_')        

class AlertDestinationNotifyOn(object):
    ON_NEVER, ON_FIRST_FAULT, ON_SECOND_FAULT, ON_THIRD_FAULT = range(4)
register_enum(AlertDestinationNotifyOn, prefix='ON_')        
        
mapper(User, t_user, properties={
    'monitors': relation(Monitor, collection_class=attribute_mapped_collection('name')),
    'destinations': relation(Destination, collection_class=attribute_mapped_collection('destination_name')),
})

mapper(Destination, t_destination, properties={
    'user': relation(User),
})

mapper(ConfirmationCode, t_confirmation)

mapper(Facet, t_facet, properties={
    'hist': relation(HistFacet, collection_class=attribute_mapped_collection('name'))
}) 

mapper(HistFacet, t_hist_facet, properties={
    'facet': relation(Facet)
}) 

mapper(Monitor, t_monitor, properties={
    'user': relation(User),
    'alert_destinations': relation(AlertDestination),
})

mapper(AlertDestination, t_alert_destination, properties={
    'monitor': relation(Monitor),
    'destination': relation(Destination)
})