#!/usr/bin/env python
import os
import logging
import sys

sys.path.append('third_party/py_interface')

from ConfigParser import ConfigParser
from authkit.authenticate import middleware as authkit_middleware

from sqlalchemy import engine_from_config
from sqlalchemy.orm import create_session

import selector
import yaro

import monimi.db
import monimi.hub
from monimi.web import get_actions, generate_smd
from monimi.controllers.main import MainController
from monimi.controllers.registration import RegistrationController
from monimi.controllers.monitor import MonitorController
from monimi.controllers.report import ReportController
from monimi.controllers.recoverpassword import PasswordRecoverController
from monimi.controllers.profile import ProfileController

log = logging.getLogger(__name__)

def start(conf, main, controllers):
    # connect to db
    monimi.db.engine = engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    monimi.db.metadata.bind = engine
    
    monimi.hub._node = monimi.hub.WebNode(conf.get('webserver', 'node_name'),
                                          conf.get('webserver', 'hub'),
                                          conf.get('webserver', 'erl_cookie'))
    monimi.hub._hub_interface = monimi.hub.HubInterface(monimi.hub._node)
    monimi.hub._node.start()

    # create WSGI app
    app = selector.Selector(wrap=yaro.Yaro)

    # main site index
    app.add('/', GET=main.index)
    app.add('/home', GET=main.home)
    app.add('/reports', GET=main.reports)
    app.add('/plans', GET=main.plans)
    app.add('/contact', GET=main.contact)
    app.add('/profile', GET=main.profile)
    app.add('/register', GET=main.register)
    app.add('/support', GET=main.support)
    app.add('/signin', POST=main.signin)
    app.add('/external_signin', POST=main.external_signin)
    app.add('/signout', POST=main.signout, GET=main.signout)
    app.add('/forgot_password', GET=main.forgot_password)
    app.add('/monibot', GET=main.monibot)
    app.add('/forum', GET=main.forum)

    # add all actions
    action_list = []
    for controller in controllers:
        for action, controller_name, name in get_actions(controller):
            url = '/controllers/%s/%s' % (controller_name, name)
            log.debug(url)
            assert action.method == 'post' 
            app.add(url, POST=action)
            action_list.append((url, action, controller_name, name))
    # add smd generator
    app.add('/controllers/smd', GET=lambda *args: generate_smd(action_list))
    return app

if __name__ == '__main__':
    conf = ConfigParser()
    conf.read('%s/etc/config' % os.environ.get('MONIMI_HOME', '.'))

    loglevel = getattr(logging, conf.get('webserver', 'loglevel').upper())
    logging.basicConfig(level=loglevel,
                        format='%(asctime)s %(thread)d %(levelname)s %(name)s %(message)s',
                        filename=os.path.join(os.environ.get('MONIMI_HOME', '.'), 'log', 'webserver.log'))
    
    logging.getLogger('authkit').setLevel('WARNING')
    
    from flup.server.fcgi import WSGIServer
    app = start(conf, MainController(conf), [
                            RegistrationController(conf.get('webserver', 're_captcha_key'),
                                                   os.path.join(os.environ.get('MONIMI_HOME', '.'), 'etc', 'GeoLiteCity.dat')),
                            MonitorController(),
                            ReportController(),
                            PasswordRecoverController(conf.get('webserver', 're_captcha_key')),
                            ProfileController(conf)])
    
    app = authkit_middleware(app,
                             setup_method='cookie',
                             cookie_includeip=False,
                             cookie_secret=conf.get('webserver', 'secret'),
                             cookie_params=dict(expires=1209600, domain='.monimi.net'),
                             cookie_signoutpath = '/signout')
    WSGIServer(app, debug=False).run()
