__version__ = "1.0"

try:
    import json
except:
    import simplejson as json

import __global as glb
import urllib
import StringIO
import logging
from copy import deepcopy
from common import sep_sina 
from urllib import urlopen, urlencode
from google.appengine.api import urlfetch
from common.utils import util
from StringIO import StringIO
from datetime import datetime
import re
from common.utils import util

_app_key = '452362856'
_app_secret = '3d6d4e5141e4ffdd6b1a9a4f69357484'
_url_header = 'https://api.weibo.com/2/%s.json'
_authorize_url = 'https://api.weibo.com/oauth2/authorize'
_redirect_uri = 'https://hfzkdebug.appspot.com/intf/sina/callback'
_access_token_url = 'https://api.weibo.com/oauth2/access_token'
_get_token_info_url = 'https://api.weibo.com/oauth2/get_token_info'

def solve_time(s):
    logging.info(s)
    try:
        index = s.index('+')
        zone = s[index:index+5]
        s = s.replace(zone,'')
        fmt = '%a %b %d %H:%M:%S  %Y'
        fmt2 = '%Y-%m-%d %H:%M:%S'
        return datetime.strptime(s, fmt).strftime(fmt2) + zone
    except:
        return s

def solve_application(s):
    return re.findall(ur'>([^<>]+)<', s)[0]

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
    
    def has_expired(self):
        try:
            param = {'access_token': self._access_token}
            res = json.load(StringIO(util.safe_call(_get_token_info_url, payload = urlencode(param), method = urlfetch.POST).content))
            if 'error_code' in res:
                if res['error_code'] in [21315,21327]:
                    return True
            else:
                return False
        except:
            return False

    def __request(self, url, data):
        
        def get(_url):
            r = StringIO(util.safe_call(_url).content)
            return r

        def post(_url, _data):
            r = StringIO(util.safe_call(_url, payload = _data, method=urlfetch.POST).content)
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
        param = {'client_id': _app_key, 'forcelogin': True, 'redirect_uri': _redirect_uri}#, 'display': 'client'}
        return _authorize_url+'?'+urllib.urlencode(param)

    def get_access_token(self, code):
        param = {'client_id': _app_key, 'client_secret': _app_secret, 'grant_type': 'authorization_code',
                   'code': code, 'redirect_uri': _redirect_uri}
        s = json.load(StringIO(util.safe_call(url=_access_token_url, payload = urlencode(param), method=urlfetch.POST).content))
        return s['access_token']
        
        
class Sina(object): 

    def __init__(self, access_token):
        self._access_token = access_token
        self.api = API(self._access_token)

    def get_info(self):
        param = {'access_token': self._access_token}
        res = json.load(StringIO(util.safe_call(_get_token_info_url, payload = urlencode(param), method = urlfetch.POST).content))
        return res
    
    def has_expired(self):
        return self.api.has_expired()
    
    def expire_in(self):
        try:
            return self.get_info()['expire_in']
        except:
            return 0
        
    def public_timeline(self, **kw):
        limit = kw['limit'] if 'limit' in kw else 64
        res = self.api.statuses__friends_timeline(count = limit)
        res = res['statuses']
        
        response = {}
        response['result'] = []
        for status in res:
            response['result'].append(self.parse_status(status))
            
        return response        
    def timeline(self, **kw):
        limit = kw['limit'] if 'limit' in kw else 64
        ID = kw['ID'] if 'ID' in kw else ''
        res = self.api.statuses__user_timeline(count = limit, ID = ID)
        res = res['statuses']
        
        response = {}
        response['result'] = []
        for status in res:
            response['result'].append(self.parse_status(status))
            
        return response
    
    def parse_status(self, status):
        response = deepcopy(glb.Status)
        logging.info(status)
        response['id'] = status['idstr']
        response['time'] = solve_time(status['created_at'])
        response['text'] = sep_sina.sep(status['text'])
        response['user'] = self.parse_user(status['user'])
        if 'original_pic' in status:
            response['image']['large_url'] = status['original_pic']
            response['image']['small_url'] = status['thumbnail_pic']
        response['favorited'] = status['favorited']
        response['comment_count'] = status['comments_count']
        response['good_count'] = status['attitudes_count']
        response['retweet_count'] = status['reposts_count']
        response['application'] = solve_application(status['source'])
        if 'retweeted_status' in status:
            response['retweet'] = self.parse_retweet(status['retweeted_status'])
            response['has_retweet'] = True
        return response
    
    def user(self, **kw):
        key = util.make_key('user_sina',kw)
        data = util.get_cache_data(key)
        if data:
            return data
        
        ID = kw['ID'] if 'ID' in kw else ''
        screen_name = kw['screen_name'] if 'screen_name' in kw else ''
        if ID:
            res = self.api.users__show(uid = ID)
        elif screen_name:
            res = self.api.users__show(screen_name = screen_name)
        
        response = self.parse_user(res)
        util.set_cache_data(key, response)
        return response
    
    def parse_retweet(self, status):
        response = deepcopy(glb.RetweetStatus)
        
        logging.info(status)
        response['id'] = status['idstr']
        response['text'] = sep_sina.sep(status['text'])
        if 'deleted' in status and status['deleted'] == '1':
            return response
        response['time'] = solve_time(status['created_at'])
        response['user'] = self.parse_user(status['user'])
        if 'original_pic' in status:
            response['image']['large_url'] = status['original_pic']
            response['image']['small_url'] = status['thumbnail_pic']
        response['favorited'] = status['favorited']
        response['comment_count'] = status['comments_count']
        response['good_count'] = status['attitudes_count']
        response['retweet_count'] = status['reposts_count']
        response['application'] = solve_application(status['source'])
        
        return response
    
    def parse_user(self, user):
        response = deepcopy(glb.User)
        response['id'] = user['idstr']
        response['screen_name'] = user['screen_name']
        response['user_name'] = user['name']
        response['follow_count'] = user['friends_count']
        response['follower_count'] = user['followers_count']
        response['profile_image']['small_url'] = user['profile_image_url']
        response['profile_image']['large_url'] = user['avatar_large']
        response['gender'] = user['gender']
        response['description'] = user['description']
        response['website'] = user['url']
        response['verified'] = user['verified']
        response['location'] = user['location']
         
        return response
    
    def friends(self, **kw):
        ID = kw['ID'] if 'ID' in kw else ''
        if not ID:
            return util.Error_Bad_Request
        
        response = []
        cursor = kw['cursor'] if 'cursor' in kw else 0
        res = self.api.friendships__friends(uid = ID, cursor=cursor)
        res = res['users']
        for user in res:
            response.append(self.parse_user(user))
            
        return {'result':response}
    
    def followers(self, **kw):
        ID = kw['ID'] if 'ID' in kw else ''
        if not ID:
            return util.Error_Bad_Request
        
        response = []
        cursor = kw['cursor'] if 'cursor' in kw else 0
        res = self.api.friendships__followers(uid = ID, cursor=cursor)
        res = res['users']
        for user in res:
            response.append(self.parse_user(user))
            
        return {'result':response}        