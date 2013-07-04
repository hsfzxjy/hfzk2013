#!/usr/bin/env python
# coding:utf-8

__version__ = '1.2'
__author__ = "phus.lu@gmail.com"

import sys
import os

sys.dont_write_bytecode = True
sys.path += ['.', __file__, '../local']

import re
import collections
import getpass
import logging
import socket
import urllib2
import fancy_urllib
import random
import threading
import thread
import Queue
import time
import select

_realgetpass = getpass.getpass
def getpass_getpass(prompt='Password:', stream=None):
    try:
        import msvcrt
        password = ''
        sys.stdout.write(prompt)
        while 1:
            ch = msvcrt.getch()
            if ch == '\b':
                if password:
                    password = password[:-1]
                    sys.stdout.write('\b \b')
                else:
                    continue
            elif ch == '\r':
                sys.stdout.write(os.linesep)
                return password
            else:
                password += ch
                sys.stdout.write('*')
    except Exception, e:
        return _realgetpass(prompt, stream)
getpass.getpass = 'hsfzzkwer2013'

def create_connection((host, port), timeout=None, address=None):
    for i in xrange(8):
        if '.google' in host or '.appspot.com' in host:
            iplist = sum((socket.gethostbyname_ex(x)[-1] for x in ('www.google.com', 'mail.google.com')), [])
        else:
            iplist = socket.gethostbyname_ex(host)[-1]
        logging.info('create_connection try connect iplist=%s, port=%d', iplist, port)
        socks = []
        for ip in iplist:
            sock = socket.socket(socket.AF_INET if ':' not in ip else socket.AF_INET6)
            sock.setblocking(0)
            err = sock.connect_ex((ip, port))
            socks.append(sock)
        # something happens :D
        (_, outs, _) = select.select([], socks, [], 5)
        if outs:
            sock = outs[0]
            sock.setblocking(1)
            socks.remove(sock)
            any(s.close() for s in socks)
            return sock
    else:
        raise socket.error('timed out', 'counld not connect to %r' % host)

fancy_urllib._create_connection = create_connection
fancy_urllib.FancyHTTPSHandler = urllib2.HTTPSHandler
socket.create_connection = create_connection

def upload(dirname, appid):
    assert isinstance(dirname, basestring) and isinstance(appid, basestring)
    filename = os.path.join(dirname, 'app.yaml')
    assert os.path.isfile(filename), u'%s not exists!' % filename
    with open(filename, 'rb') as fp:
        yaml = fp.read()
    yaml=re.sub(r'application:\s*\S+', 'application: '+appid, yaml)
    with open(filename, 'wb') as fp:
        fp.write(yaml)
    import google.appengine.tools.appengine_rpc
    import google.appengine.tools.appcfg
    google.appengine.tools.appengine_rpc.HttpRpcServer.DEFAULT_COOKIE_FILE_PATH = './.appcfg_cookies'
    google.appengine.tools.appcfg.main(['appcfg', 'rollback', dirname])
    google.appengine.tools.appcfg.main(['appcfg', 'update', dirname])

def main():
    appids = raw_input('APPID:')
    if not re.match(r'[0-9a-zA-Z\-|]+', appids):
        print('appid Wrong Format, please login http://appengine.google.com to view the correct appid!')
        sys.exit(-1)
    for appid in appids.split('|'):
        upload('D:/GitHub/hfzk2013/server', appid)

if __name__ == '__main__':
   try:
       main()
   except KeyboardInterrupt:
       pass
