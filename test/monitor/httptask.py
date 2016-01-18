from twisted.trial import unittest
import twisted.internet.error as ineterror
import twisted.web.error as weberror
import monimi.monitor.error as error
from monimi.monitor.httptask import HttpUrlTask
from monimi.monitor.common import MonitoringSuccess

babble = "http://localhost:7070"

class DummyMonitor(object):
    def __init__(self, url):
        self.url = babble + url

class TestCase(unittest.TestCase):
    def failUnlessMonitoringSuccess(self, a):
        self.failUnlessEqual(a.__class__, MonitoringSuccess)
    
    def failUnlessErrorType(self, a, b):
        self.failUnlessEqual(a.reason.type, b)

class Simple(TestCase):
    def test_hello(self):
        task = HttpUrlTask(DummyMonitor('/py/hello'))
        d = task.start()
        d.addCallback(self.failUnlessMonitoringSuccess)        
        return d

class Redirects(TestCase):
    def test_redirect_fails(self):
        task = HttpUrlTask(DummyMonitor('/py/redirect/1'))
        task.max_redirects = 0
        d = task.start()
        d.addCallback(self.failUnlessErrorType, error.RedirectNotAllowedError)        
        return d

    def test_redirect_success(self):
        task = HttpUrlTask(DummyMonitor('/py/redirect/1'))
        task.protocol.followRedirect = 1
        d = task.start()
        d.addCallback(self.failUnlessMonitoringSuccess)        
        return d

    def test_redirect_too_many(self):
        task = HttpUrlTask(DummyMonitor('/py/redirect/5'))
        task.protocol.followRedirect = 1
        task.protocol.max_redirects = 2
        d = task.start()
        d.addCallback(self.failUnlessErrorType, error.TooManyRedirectsError)        
        return d

    def test_redirect_to_ssl(self):
        task = HttpUrlTask(DummyMonitor('/py/redirect_to_ssl'))
        task.protocol.followRedirect = 1
        task.protocol.max_redirects = 2
        task.protocol.allow_ssl = False
        d = task.start()
        d.addCallback(self.failUnlessErrorType, error.SslNotAllowedError)        
        return d
    
class Timeout(TestCase):
    def test_timeout_ok(self):
        task = HttpUrlTask(DummyMonitor('/py/sleep/2'))
        d = task.start()
        d.addCallback(self.failUnlessMonitoringSuccess)        
        return d

    def test_timeout_fail(self):
        task = HttpUrlTask(DummyMonitor('/py/sleep/2'), timeout=1)
        d = task.start()
        d.addCallback(self.failUnlessErrorType, ineterror.TimeoutError)        
        return d