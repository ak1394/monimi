_max_monitor_count =    dict(free=3,        basic=5,        pro=15,         premium=30,     dev=100)
_min_beat =             dict(free=20,       basic=5,        pro=1,          premium=1,      dev=1)
_is_https_allowed =     dict(free=False,    basic=True,     pro=True,       premium=True,   dev=True)
_sms_per_month =        dict(free=5,        basic=15,       pro=30,         premium=70,     dev=100)

def max_monitor_count(user):
    return _max_monitor_count[str(user.plan)]

def is_https_allowed(user):
    return _is_https_allowed[str(user.plan)]
    
def min_beat(user):
    return _min_beat[str(user.plan)]

def get_plan_credits(plan):
    return _sms_per_month[str(plan)]
