import logging
import monimi.limits as limits
from monimi import db

log = logging.getLogger(__name__)

class AddMonitor():
    def check(self, request):
        log.debug("%s %s %s", request.user, request.user.plan, len(request.user.monitors))
        if request.user and len(request.user.monitors) < limits.max_monitor_count(request.user):
            return True
        return False        

class HttpsAllowed():
    def check(self, request):
        log.debug("%s %s %s", request.user, request.user.plan, len(request.user.monitors))
        return request.user and limits.is_https_allowed(request.user)

class ValidUser():
    def check(self, request):
        if request.environ.get('REMOTE_USER'):
            log.debug("checking for user: %s" % request.environ.get('REMOTE_USER'))
            user = request.session.query(db.User).filter_by(email=request.environ.get('REMOTE_USER').lower()).first()
            if user:
                request.user = user
                return True
            else:
                request.environ['paste.auth_tkt.logout_user']()
                return False
    
class OwnerOfMonitor():
    def __init__(self, monitor_name_param):
        self.name = monitor_name_param
        
    def check(self, request):
        return True
        #log.debug("checking if user %s owns monitor: %s" % request.user)
        #return hasattr(request, 'user') and request.user    