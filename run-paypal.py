#!/usr/bin/env python
import os
import logging
import sys
from ConfigParser import ConfigParser

from flup.server.fcgi import WSGIServer
from sqlalchemy import engine_from_config
import selector
import yaro

import monimi.db
from monimi import paypal

if __name__ == '__main__':
    conf = ConfigParser()
    conf.read('%s/etc/config' % os.environ.get('MONIMI_HOME', '.'))

    loglevel = getattr(logging, conf.get('paypal-ipn', 'loglevel').upper())
    logging.basicConfig(level=loglevel,
                        format='%(asctime)s %(thread)d %(levelname)s %(name)s %(message)s',
                        filename=os.path.join(os.environ.get('MONIMI_HOME', '.'), 'log', 'paypal.log'))

    monimi.db.engine = engine = engine_from_config(dict(conf.items('sqlalchemy')), prefix='')
    monimi.db.metadata.bind = engine
    
    paypal.config = conf

    app = selector.Selector(wrap=yaro.Yaro)
    
    app.add('/ipn', POST=paypal.ipn)
    app.add('/payment', GET=paypal.pdt)
#    app.add('/subscription_modified', GET=paypal.subscription_modified)
    
    WSGIServer(app, debug=False).run()