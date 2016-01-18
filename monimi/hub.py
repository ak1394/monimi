import threading, thread
import logging 
import functools
from Queue import Queue
from traceback import format_exc

from py_interface import erl_node, erl_eventhandler, erl_opts, erl_term

log = logging.getLogger(__name__)

class MessageQueue(object):
    def __init__(self):
        self.queue = Queue()

    def cast(self, to, message):
        to.queue.put_nowait((self, message))
    
    def call(self, to, message, timeout=60):
        self.cast(to, message)
        frm, result = self.queue.get(block=True, timeout=timeout)
        return result

    def messages(self):
        while not self.queue.empty():
            frm, message = self.queue.get()
            yield frm, message

class HubInterface(object):
    def __init__(self, web_node):
        self.web_node = web_node
        
    def _cast(self, message):
        mq = MessageQueue()
        mq.cast(self.web_node.mq, message)

    def _call(self, message):
        mq = MessageQueue()
        return mq.call(self.web_node.mq, message)

    def smsStatus(self, sms_id):
        return self._call(('sms_status', sms_id))
    
    def touchMonitor(self, monitor_id):
        self._cast(('touch_check', monitor_id))
    
    def sendConfirmation(self, phone_no, confirmation):
        return self._call(('send_confirmation', str(phone_no), str(confirmation)))

    def remindPassword(self, email, password):
        self._cast(('remind_password', str(email), str(password)))

    def sendWelcome(self, email, password):
        self._cast(('send_welcome', str(email), str(password)))

    def getNameservers(self, name):
        return self._call(('get_nameservers', name))

class WebNode(threading.Thread):
    def __init__(self, node_name, hub_name, cookie, timer_tick=1):
        threading.Thread.__init__(self)
        self.setDaemon(True)
        self.timer_tick = timer_tick
        self.node = erl_node.ErlNode(node_name, erl_opts.ErlNodeOpts(cookie=cookie))
        self.hub_name = hub_name
        self.evhand = erl_eventhandler.GetEventHandler()
        self.mailbox_pool = []
        self.mq = MessageQueue()
        self.evhand.AddTimerEvent(self.timer_tick, self._timerCallback)

    def run(self):
        self.node.Publish()
        self.evhand.Loop()

    def _prepare_message(self, mailbox, message):
        prepared_message = [mailbox.Self(), erl_term.ErlAtom(message[0])]
        if len(message) > 1:
            for value in message[1:]:
                prepared_message.append(value)
        return tuple(prepared_message)

    def _get_mailbox(self, mq):
        if self.mailbox_pool:
            mailbox = self.mailbox_pool.pop()
        else:
            mailbox = self.node.CreateMBox()
        mailbox._msgCallback = functools.partial(self._mboxCallback, mq, mailbox)
        return mailbox

    def _free_mailbox(self, mailbox):
        mailbox._msgCallback = mailbox._Sink
        self.mailbox_pool.append(mailbox)

    def _timerCallback(self):
        try:
            for frm, message in self.mq.messages(): 
                log.debug("sending message to hub: %s", message)
                mailbox = self._get_mailbox(frm)
                mailbox.Send(('mm_hub', self.hub_name), self._prepare_message(mailbox, message))
            self.evhand.AddTimerEvent(self.timer_tick, self._timerCallback)
        except:
            log.error("exception in hub timer callback %s", format_exc())
 
    def _mboxCallback(self, mq, mailbox, message, *k, **kw):
        try:
            log.debug("mbox callback: %s %s, %s %s %s", mq, mailbox, message, k, kw)        
            self.mq.cast(mq, message)
            self._free_mailbox(mailbox)
        except:
            log.error("exception in hub mailbox callback %s", format_exc())