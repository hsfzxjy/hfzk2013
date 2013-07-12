import base64
import json
import urllib
import webapp2
from google.appengine.ext import db
from google.appengine.api import urlfetch
from google.appengine.runtime import DeadlineExceededError

def safe_call(url, payload = None, method = urlfetch.GET):
    
    def _call(_url, _payload = None, _method = urlfetch.GET):
        try:
            res = urlfetch.fetch(_url, payload = _payload, method = _method, \
                                 deadline = 10)
        except DeadlineExceededError:
            res = None
        return res
    
    res = None
    while not res:
        res = _call(url, _payload = payload, _method = method)
    return res

def login_data_encode(data):
    result = ''
    data["access_token"] = base64.encodestring(data["access_token"])
    result = urllib.urlencode(data)
    return result

def login_data_decode(data):
    result = {}
    for arg in data.arguments():
        result[arg] = data.get(arg)
    result["access_token"] = base64.decodestring(result["access_token"])
    return result

def redirect_to_login(cls, data):
    cls.redirect('/user/login?'+login_data_encode(data))
    
def urldecode(url):
    return {urllib.unquote(s.split('=')[0]):urllib.unquote(s.split('=')[1]) for s in url.split('&')}