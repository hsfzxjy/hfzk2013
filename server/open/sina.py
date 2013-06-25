__version__ = "1.0"

try:
    import json
except:
    import simplejson as json

import __global
import urllib
import StringIO
import logging
from urllib import urlopen, urlencode

_app_key = '452362856'
_app_secret = '3d6d4e5141e4ffdd6b1a9a4f69357484'
_url_header = 'https://api.weibo.com/2/%s.json'
_authorize_url = 'https://api.weibo.com/oauth2/authorize'
_redirect_uri = 'http://127.0.0.1:11080/intf/sina/callback'#'https://api.weibo.com/oauth2/default.html'
_access_token_url = 'https://api.weibo.com/oauth2/access_token'
_get_token_info_url = 'https://api.weibo.com/oauth2/get_token_info'

class APIError(StandardError):
    
    def __init__(self, _json= None):
        if isinstance(_json, str):
            s = json.load(StringIO.StringIO(_json))
        else:
            s = _json
        self.request = s["request"]
        self.error_code = s["error_code"]
        self.error_reason = s["error"]
        
    def __str__(self):
        return '%d : %s'%(self.error_code, self.error_reason)
    
class API(object):

    def __init__(self, access_token):
        self._access_token = access_token

    def __request(self, url, data):
        
        def get(_url):
            r = urlopen(_url)
            return r

        def post(_url, _data):
            r = urlopen(_url, _data)
            return r

        def test_method(s):
            result = False
            try:
                if s.has_key('error_code'):
                    raise APIError(s)
            except APIError, e:
                if e.error_code == 10021:
                    result = True
            return result

        res = json.load(get(url+'?'+data))
        if test_method(res):
            res = json.load(post(url, data))
        return res

    def __getattr__(self, attr):
        
        def wrap(**kw):
            _url = _url_header % attr.replace('__','/')
            kw['access_token'] = self._access_token
            for i,j in kw.iteritems():
                if isinstance(j,unicode):
                    kw[i] = j.encode('utf8')
            logging.info(str(kw))
            return self.__request(_url, urlencode(kw))

        return wrap

class OAuth(object):

    def get_authorize_url(self):
        param = {'client_id': _app_key, 'forcelogin': True, 'redirect_uri': _redirect_uri}
        return _authorize_url+'?'+urllib.urlencode(param)

    def get_access_token(self, code):
        param = {'client_id': _app_key, 'client_secret': _app_secret, 'grant_type': 'authorization_code',
                   'code': code, 'redirect_uri': _redirect_uri}
        s = json.load(urllib.urlopen(_access_token_url, urlencode(param)))
        return s['access_token']
        
        
class Sina(object): 

    def __init__(self, access_token):
        self._access_token = access_token
        self.api = API(self._access_token)

    def get_info(self):
        param = {'access_token': self._access_token}
        res = json.load(urlopen(_get_token_info_url, urlencode(param)))
        return res