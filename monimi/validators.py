import formencode
import re
import pytz
from formencode import validators

email_maxlen = 64
monitor_name_maxlen = 24
url_maxlen = 128

class TimeZone(validators.FancyValidator):
    not_empty = True
    messages = dict(unknown_timezone="Unknown timezone")

    def _to_python(self, value, state):
        return value.strip()

    def validate_python(self, value, state):
        if value not in pytz.common_timezones:
            raise formencode.Invalid(self.message("unknown_timezone", state), value, state)

class CheckName(validators.Regex):
    regex = r"^[a-zA-Z_\-0-9\.]*$"
    not_empty=True
    min=1
    max=monitor_name_maxlen
    strip=True    
    messages = {
        'invalid': 'Enter only letters, numbers, . (dot), _ (underscore), - (hyphen)',
        }

class RelaxedPlainText(validators.Regex):
    regex = r"^[a-zA-Z_\-0-9\.]*$"
    not_empty=True
    strip=True    
    messages = {
        'invalid': 'Enter only letters, numbers, . (dot), _ (underscore), - (hyphen)',
        }

class MyEmail(validators.Email):
    def _to_python(self, value, state):
        return validators.Email._to_python(self, str(value), state)

class PhoneNumber(validators.FancyValidator):
    not_empty = True
    
    non_digits_regex = re.compile(r'\D')
    messages = dict(too_short="Phone number is too short, please enter in international format with country code",
                    too_long="Phone number is too long")

    def _to_python(self, value, state):
        return ''.join(self.non_digits_regex.split(value))

    def validate_python(self, value, state):
        if len(value) < 9:
            raise formencode.Invalid(self.message("too_short", state), value, state)
        elif len(value) > 16:
            raise formencode.Invalid(self.message("too_long", state), value, state)

# main
class SigninSchema(formencode.Schema):
    email = validators.Email(resolve_domain=False, not_empty=True, max=email_maxlen)
    password = validators.String(not_empty=True, min=4, max=16)
    inputsubmit1 = validators.String()

# registration
class RegistrationSchema(formencode.Schema):
    email = MyEmail(resolve_domain=True, not_empty=True, max=email_maxlen)
    password = validators.String(not_empty=True, min=4, max=16)
    confirm = validators.String()
    mobile = PhoneNumber()
    captcha_challenge = validators.String(not_empty=True, max=512)
    captcha_response = validators.String(not_empty=True, max=64)
    chained_validators = [validators.FieldsMatch('password', 'confirm')]

class ConfirmationSchema(formencode.Schema):
    confirmation_code = validators.String(not_empty=True, max=16)
    timezone = TimeZone()

# monitors
class MonitorAddSchema(formencode.Schema):
    name = CheckName()
    beat = validators.Int(not_empty=True, min=1, max=60)
    url = validators.URL(not_empty=True, min=1, max=url_maxlen)

class MonitorAddPingSchema(formencode.Schema):
    name = CheckName()
    beat = validators.Int(not_empty=True, min=1, max=60)
    host = validators.String(not_empty=True, min=4, max=64)

class MonitorAddDnsSchema(formencode.Schema):
    name = CheckName()
    beat = validators.Int(not_empty=True, min=1, max=60)
    hostname = validators.String(not_empty=True, min=4, max=64)
    qtype = validators.OneOf(['a', 'cname', 'mx', 'ns', 'soa'], if_missing=None, if_empty=None)
    ns = validators.String(if_missing=None, if_empty=None, min=4, max=64)

class MonitorSettingsSchema(formencode.Schema):
    name = CheckName()
    new_name = CheckName()
    beat = validators.Int(not_empty=True, min=1, max=60)

class MonitorNameSchema(formencode.Schema):
    name = CheckName()
    
# recover password
class RecoverPasswordSchema(formencode.Schema):
    email = validators.Email(resolve_domain=False, not_empty=True, max=email_maxlen)
    captcha_challenge = validators.String(not_empty=True, max=512)
    captcha_response = validators.String(not_empty=True, max=64)
    
# reports
class ReportSchema(formencode.Schema):
    monitor = CheckName()
    date_for = validators.String(not_empty=True)
    
class ReportNameSchema(formencode.Schema):
    monitor = CheckName()

# profile
class ChangePasswordSchema(formencode.Schema):
    old_password = validators.String(not_empty=True, min=4, max=16)
    new_password = validators.String(not_empty=True, min=4, max=16)
    confirm_password = validators.String()
    chained_validators = [validators.FieldsMatch('new_password', 'confirm_password')]

class ChangeUserSchema(formencode.Schema):
    first_name = validators.String(not_empty=False, min=1, max=32)
    last_name = validators.String(not_empty=False, min=1, max=32)
    username = formencode.All(RelaxedPlainText(),
                              validators.MinLength(2),
                              validators.MaxLength(16))

class ChangeTimezoneSchema(formencode.Schema):
    timezone = TimeZone()
