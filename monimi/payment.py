import logging
import urllib2
from urllib import urlencode, unquote_plus
from cgi import parse_qs 
from sqlalchemy import *
from monimi.db import t_paypal_payments
import simplejson
from M2Crypto import BIO, SMIME, X509
from subprocess import Popen, PIPE
import os

log = logging.getLogger(__name__)

class PaypalOrder(dict):
    """Acts as a dictionary which can be encrypted to Paypal's EWP service"""
    def __init__(self, **kwargs):
        dict.__init__(self, **kwargs)

    def plaintext(self):
        plain = '\n'.join(['%s=%s' % (key, value) for key, value in self.items()])
        return plain.encode('utf-8')

    __str__ = plaintext

    def encrypt(self, config):
        key = os.path.join(os.environ.get('MONIMI_HOME', '.'), config.get('paypal', 'key'))
        cert = os.path.join(os.environ.get('MONIMI_HOME', '.'), config.get('paypal', 'cert'))
        paypal_cert = os.path.join(os.environ.get('MONIMI_HOME', '.'), config.get('paypal', 'paypal_cert'))
        self['cert_id'] = config.get('paypal', 'cert_id')
        
        # Instantiate an SMIME object.
        s = SMIME.SMIME()

        # Load signer's key and cert. Sign the buffer.
        s.load_key_bio(BIO.openfile(key), BIO.openfile(cert))

        p7 = s.sign(BIO.MemoryBuffer(self.plaintext()), flags=SMIME.PKCS7_BINARY)

        # Load target cert to encrypt the signed message to.
        x509 = X509.load_cert_bio(BIO.openfile(paypal_cert))
        sk = X509.X509_Stack()
        sk.push(x509)
        s.set_x509_stack(sk)

        # Set cipher: 3-key triple-DES in CBC mode.
        s.set_cipher(SMIME.Cipher('des_ede3_cbc'))

        # Create a temporary buffer.
        tmp = BIO.MemoryBuffer()

        # Write the signed message into the temporary buffer.
        p7.write_der(tmp)

        # Encrypt the temporary buffer.
        p7 = s.encrypt(tmp, flags=SMIME.PKCS7_BINARY)

        # Output p7 in mail-friendly format.
        out = BIO.MemoryBuffer()
        p7.write(out)

        return out.read()
    

def is_unique_transaction(txn_id, session):
    query = select([t_paypal_payments.c.txn_id], t_paypal_payments.c.txn_id == txn_id)  
    return len(session.execute(query).fetchall()) == 0

def record_transaction(user_id, txn_id, data, session):
    session.execute(t_paypal_payments.insert(values={'user_id':user_id, 'txn_id':txn_id, 'data':simplejson.dumps(data)}))
    
def verify_pdt(tx, session, config):
    pp_url = config.get('paypal', 'url')
    pdt_token = config.get('paypal', 'pdt_token')
    
    params = dict(cmd='_notify-synch', tx=tx,
                  at=pdt_token)
    
    req = urllib2.Request(pp_url)
    req.add_header("Content-type", "application/x-www-form-urlencoded")
    ret = urllib2.urlopen(pp_url, urlencode(params)).read()
    result_lines = ret.splitlines()
    if result_lines[0] == 'SUCCESS':
        result = dict()
        for line in result_lines[1:]:
            name, value = line.split('=')
            name = unquote_plus(name)
            value = unquote_plus(value)
            result[name] = value
        return result
    else:
        return None
    
def verify_ipn(ipn_params, config):
    pp_url = config.get('paypal', 'url')
    params = dict()
    for key, value in ipn_params.items():
        params[key] = value
    params['cmd'] = '_notify-validate'
    req = urllib2.Request(pp_url)
    req.add_header("Content-type", "application/x-www-form-urlencoded")
    ret = urllib2.urlopen(pp_url, urlencode(params)).read()
    return ret == 'VERIFIED'
