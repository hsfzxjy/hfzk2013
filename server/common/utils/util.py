import base64
import json
import urllib
import webapp2
import time
import code
from google.appengine.ext import db
from google.appengine.api import urlfetch, urlfetch_errors
from google.appengine import runtime
from google.appengine.runtime import apiproxy_errors
from google.appengine.api import memcache as cache

DeadlineErrors = (runtime.DeadlineExceededError, apiproxy_errors.DeadlineExceededError,
                  urlfetch_errors.DeadlineExceededError)

#Error Structures
Error_Server_Busy = {'_error': 'Server busy!', '_error_code': 0}
Error_Bad_Request = {'_error': 'Bad request!', '_error_code': 1}
Error_Expired = {'_error': 'The token has expired!', '_error_code':2}

def make_key(category, data):
    return '%s_%s' % (category, '_'.join(['='.join(i) for i in data.iteritems()]))

def set_cache_data(key, value):
    try:
        data = code.object_to_json(value)
        cache.set(key = key, value = data, time = 3600)
    except:
        return False
    return True

def get_cache_data(key):
    data = cache.get(key)
    if data:
        data = code.json_to_object(data)
    return data

def safe_call(url, payload = None, method = urlfetch.GET):
    
    def _call(_url, _payload = None, _method = urlfetch.GET):
        try:
            res = urlfetch.fetch(_url, payload = _payload, method = _method, \
                                 deadline = 10)
        except DeadlineErrors:
            res = None
            time.sleep(0.5)
        
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