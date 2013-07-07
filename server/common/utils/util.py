import base64
import json
import urllib
import webapp2
from google.appengine.ext import db

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