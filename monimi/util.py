import string
import time
from datetime import datetime
from pytz import utc

enum_keys = dict()
enum_values = dict()

def register_enum(klass, prefix, name=''):
    values = {}
    keys = {}
    for key, val in klass.__dict__.items():
        if key.startswith(prefix):
            values[val] = key
            keys[key] = val
    enum_keys[str(klass) + name] = keys
    enum_values[str(klass) + name] = values
    
def encode_enum(klass, value, name=''):
    return enum_values[str(klass) + name][value]
        
def decode_enum(klass, key, name=''):
    return enum_keys[str(klass) + name][key]

def utcdatetimefromiso(isodatetime, timezone):
    if isodatetime.endswith('Z'):
        return timezone.localize(datetime.strptime(isodatetime[:-1], "%Y-%m-%dT%H:%M:%S")).astimezone(utc)
    elif '+' in isodatetime:
        return timezone.localize(datetime.strptime(isodatetime.split('+', 1)[0], "%Y-%m-%dT%H:%M:%S")).astimezone(utc)
    else:
        return timezone.localize(datetime.strptime(isodatetime.rsplit('-', 1)[0], "%Y-%m-%dT%H:%M:%S")).astimezone(utc)

def datefromiso(isodatetime):
    if isodatetime:
        return datetime.strptime(isodatetime.split('T', 1)[0], "%Y-%m-%d").date()
    return None

def localtimefromtime(time, timezone):
    if time:
        return datetime.combine(datetime.now(utc), time).replace(tzinfo=utc).astimezone(timezone).time()

def ts_to_datetime(ts, tz=utc):
    return datetime.fromtimestamp(ts, tz)    

def datetime_to_ts(date_time):
    return long(time.mktime(date_time.timetuple()))

def ts_now():
    return datetime_to_ts(datetime.utcnow())

def get_utc_offset(tz, now=None):
    now = datetime.now() if now is None else now
    tz_now = tz.localize(now)
    utc_now = utc.localize(now)
    offset = utc_now - tz_now
    offset_seconds = offset.seconds + offset.days * (24 * 3600)
    return int(offset_seconds / 60)

abc = string.letters

def base(number, radix):
   """base(number, radix)
         inverse function to int(str,radix) and long(str,radix)
   """

   if not 2 <= radix <= 36:
      raise ValueError, "radix must be in 2..36"

   result = []
   addon = result.append
   if number < 0:
      number = -number
      addon('-')
   elif number == 0:
      addon('0')

   _divmod, _abc = divmod, abc
   while number:
      number, rdigit = _divmod(number, radix)
      addon(_abc[rdigit])

   result.reverse()
   return ''.join(result)


def encrypt_password(password):
    return password