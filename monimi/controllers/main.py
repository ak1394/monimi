import logging
from urlparse import urlsplit
from datetime import datetime

from sqlalchemy import and_, or_
from genshi.template import TemplateLoader

from monimi.db import User
from monimi.web import get, post, authorize
from monimi.validators import SigninSchema 
import monimi.permissions as perm

log = logging.getLogger(__name__)

class MainController(object):
    def __init__(self, config):
        self.config = config
        self.loader = TemplateLoader('templates', auto_reload=True)
    
    @get()
    def index(self, r):
        if r.environ['wsgi.url_scheme'] != 'https':
            return self._render(r, 'welcome.html')
        else:
            r.redirect('http://%s/' % r.environ['SERVER_NAME'])
    
    @get()
    def home(self, r):
        if perm.ValidUser().check(r):
            r.user.access = datetime.utcnow()
            r.session.add(r.user)
            r.session.flush()
            return self._render(r, 'home.html')
        else:
            return self._render(r, 'logged_out.html')

    @get()
    def register(self, r):
        if r.environ['wsgi.url_scheme'] == 'https':
            return self._render(r, 'register.html')
        else:
            r.redirect('https://%s/register' % r.environ['SERVER_NAME'])

    @get()
    def plans(self, r):
        return self._render(r, 'plans.html')

    @get()
    def contact(self, r):
        return self._render(r, 'contact.html')

    @get()
    def forgot_password(self, r):
        return self._render(r, 'forgot_password.html')

    @post()
#    @post(validator=SigninSchema())
    def signin(self, r, **kwargs):
        email = r.form['email'].lower()
        password = r.form['password']
        user = r.session.query(User).filter(or_(
            and_(User.email==email, User.password==password),
            and_(User.username==email, User.password==password))).first()
        if user:
            user.login = datetime.utcnow()
            r.session.add(user)
            r.session.flush()
            r.environ['paste.auth_tkt.set_user'](userid=user.email)
            r.redirect('http://%s/home' % r.environ['SERVER_NAME'])
        else:
            return self._render(r, 'signin_failed.html')

    @post()
    def external_signin(self, r, **kwargs):
        name = r.form['name'].lower()
        password = r.form['pass']
        user = r.session.query(User).filter(and_(User.email==name, User.password==password)).first()
        if user:
            r.environ['paste.auth_tkt.set_user'](userid=name)
            if r.environ['HTTP_REFERER']:
                referer = urlsplit(r.environ['HTTP_REFERER'])
                r.redirect('%s://%s%s' % (referer[0], referer[1], referer[2]))
            else:
                r.redirect('http://%s/' % r.environ['SERVER_NAME'])
        else:
            return self._render(r, 'signin_failed.html')

    @get()
    def reports(self, r):
        if perm.ValidUser().check(r):
            return self._render(r, 'reports.html')
        else:
            return self._render(r, 'logged_out.html')

    @get()
    def profile(self, r):
        if perm.ValidUser().check(r):
            return self._render(r, 'profile.html')
        else:
            return self._render(r, 'logged_out.html')

    @get()
    def support(self, r):
        if perm.ValidUser().check(r):
            return self._render(r, 'support.html')
        else:
            return self._render(r, 'logged_out.html')

    @get()
    def forum(self, r):
        if perm.ValidUser().check(r):
            if not r.user.username:
                return self._render(r, 'please_set_username.html')
        r.redirect('http://forum.monimi.net/')

    @get()
    def signout(self, r, **kwargs):
        r.redirect('/')

    @get()
    def monibot(self, r):
        if r.environ['wsgi.url_scheme'] != 'https':
            return self._render(r, 'monibot.html')
        else:
            r.redirect('http://%s/monibot' % r.environ['SERVER_NAME'])

    def _render(self, r, template_name):
        template = self.loader.load(template_name)
        
        vars = dict(server_name=r.environ['SERVER_NAME'],
                    logged_in=r.environ.has_key('REMOTE_USER'))

        if hasattr(r, 'user') and r.user:
            vars['ads'] = r.user.show_ads()
        
        return template.generate(**vars).render('html', doctype='html')
