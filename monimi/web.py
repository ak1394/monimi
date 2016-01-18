import os
import logging
import datetime
from inspect import getargspec, getmembers, ismethod
from contextlib import closing
from traceback import format_exc
from decorator import decorator, new_wrapper

from sqlalchemy import engine_from_config
from sqlalchemy.orm import create_session

import selector
import yaro
import simplejson
import formencode

import monimi.db
import monimi.hub

log = logging.getLogger(__name__)

class ValidationFailedException(Exception):
    def __init__(self, detail):
        Exception.__init__(self, 'failed to validate input')
        self.detail = detail

class PermissionDeniedException(Exception):
    def __init__(self, message, klass):
        Exception.__init__(self, message)
        self.klass = klass

def get():
    def _get(action):
        def _get_inner(self, request):
            #log.debug("calling: %s/%s", self.__class__.__name__, action.__name__)
            result = _run_action(self, action, request, {})
            #log.debug("result: %s", str(result)[:120])
            return result
        _get_inner.method = 'get'
        return _get_inner
    return _get

def post(validator=None):
    def _post(action):
        def _post_inner(self, request):
            try:
                assert request.content_type.startswith('application/x-www-form-urlencoded')
                log.debug("calling: %s/%s %s", self.__class__.__name__, action.__name__)
                newargs = _validate(validator, request.form)
                log.debug("args: %s", newargs)
                result = _run_action(self, action, request, newargs)
                log.debug("result: %s", str(result)[:120])
                return result
            except Exception, e:
                log.error("exception %s", format_exc())
                raise e
        _post_inner.method = 'post'
        return _post_inner
    return _post

def jrpc(validator=None):
    def _jrpc(action):
        def _jrpc_inner(self, request):
            try:
                assert request.content_type.startswith('application/json')
                request.res.headers['Content-Type'] = 'application/json'
                args = dict([(str(key), val) for key, val in _dejsonify(request.body).items()])
                newargs = _validate(validator, args)
                log.debug("calling: %s/%s %s", self.__class__.__name__, action.__name__, newargs)
                result = _run_action(self, action, request, newargs)
                log.debug("result: %s", str(result)[:120])
                return _jsonify(result)
            except ValidationFailedException, e:
                return _jsonify(dict(error='validation_failed', detail=e.detail))
            except Exception, e:
                log.error("exception %s", format_exc())
                return _jsonify(dict(error='exception'))
        _jrpc_inner.method = 'post'
        _jrpc_inner.jrpc = True 
        return _jrpc_inner
    return _jrpc

def authorize(*permissions):
    @decorator
    def _authorize(action, *args, **kwargs):
        for permission in permissions:
            permitted = permission.check(args[1]) # pass 'request' argument
            log.debug("permission: %s %s", permission.__class__.__name__, permitted)
            if not permitted: # second argument is the request XXX FIXME might be bad, maybe use kwargs/arg names instead of positions?
                raise PermissionDeniedException("Permission '%s' is denied" % permission.__class__.__name__, permission.__class__)
        return action(*args, **kwargs)
    return _authorize

def get_actions(controller):
    action_list = list()
    controller_name = controller.__module__.split('.')[-1] # expects something like culture.module.addressbook.module in __module__
    for name, m in getmembers(controller, ismethod):
        if hasattr(m, 'jrpc') and m.jrpc:
            action_list.append((m, controller_name, name))
    return action_list

def generate_smd(action_list):
    smd = dict(SMDVersion='.1', objectName='monimi', serviceType='JSON-P', methods=[])
    for url, action, controller_name, name in action_list:
        if action.jrpc:
            method_name = controller_name + ''.join([part.capitalize() for part in name.split('_')])
            method = dict(name=method_name, serviceURL=url, parameters=[])
            smd['methods'].append(method)
    return _jsonify(smd)


def _validate(validator, args):
    if not validator:
        return args
    try: 
        return validator.to_python(args)
    except formencode.Invalid, e:
        log.error("validation failed %s %s", e, args)
        raise ValidationFailedException(e.unpack_errors())

def _run_action(self, action, request, args):
    try:
        request.connection = connection = monimi.db.engine.connect()
        request.session = session = create_session(bind=connection)
        request.hub = monimi.hub._hub_interface
        transaction = connection.begin()
        response = action(self, request, **args)
        transaction.commit()
        return response
    except:
        log.error("exception %s", format_exc())
        transaction.rollback()
        raise
    finally:
        session.close()
        connection.close()

class Encoder(simplejson.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, datetime.datetime):
            return obj.strftime('%Y-%m-%dT%H:%M:%S')
        elif isinstance(obj, datetime.date):
            return obj.strftime('%Y-%m-%d')
        elif isinstance(obj, datetime.time):
            return obj.strftime('T%H:%M:%S')
        return simplejson.JSONEncoder.default(self, obj)
            
def _jsonify(data):
    if data is None:
        data = dict()
    return '{}&&' + simplejson.dumps(data, cls=Encoder)

def _dejsonify(data):
    return simplejson.loads(data)
