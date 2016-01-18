import logging
import random
import recaptcha.client.captcha as captcha
import formencode
from monimi import db
from monimi.web import jrpc, ValidationFailedException
from monimi import util
from monimi import validators

log = logging.getLogger(__name__)

class PasswordRecoverController(object):
    def __init__(self, captcha_key):
        self.captcha_key = captcha_key

    @jrpc(validator=validators.RecoverPasswordSchema())
    def recover_password(self, r, email, captcha_challenge, captcha_response):
        remote_ip = r.environ['REMOTE_ADDR']
        captcha_result = captcha.submit(captcha_challenge, captcha_response, self.captcha_key, remote_ip)
        if captcha_result.is_valid:
            user = r.session.query(db.User).filter_by(email=email.lower()).first()
            if user:
                log.debug("password reminder for user: %s", user.email)            
                r.hub.remindPassword(email, user.password)
            else:
                log.debug("no user %s found", email)
        else:
            raise ValidationFailedException(dict(captcha_response='Validation failed'))