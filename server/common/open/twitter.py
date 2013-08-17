# -*- coding: cp936 -*-

__version__ = '1.0'

CONSUMER_KEY = '3nVuSoBZnx6U4vzUxf5w'#'9lY1CjPgEiQTMNXdYnEiw'
CONSUMER_SECRET = 'Bcs59EFbbsdF6Sl9Ng71smgStWEGwXXKSjYvPVt7qys'#'hIvCLhG6u37RUfjYI7wa6bHDbjjgGZx6h5S37LTwE'
REQUEST_TOKEN_URL = 'https://api.twitter.com/oauth/request_token'
ACCESS_TOKEN_URL = 'https://api.twitter.com/oauth/access_token'
AUTHORIZE_URL = 'https://api.twitter.com/oauth/authorize'
CALLBACK_URL = 'https://hfzkdebug.appspot.com/intf/twitter/callback'
REST_BASE_URL = 'https://api.twitter.com/1.1/%s.json'
STREAM_BASE_URL = 'https://stream.api.com/1.1/%s.json'
SIGNIN_URL = 'https://api.twitter.com/oauth/authenticate'

try:
    from urlparse import parse_qsl
except:
    from cgi import parse_qsl
    
import oauth2 as oauth
from google.appengine.ext import db
import _twitter
import urllib
import logging
from urllib import urlencode,quote
import hmac
from hashlib import sha1
from random import getrandbits
from time import time
from json import load
from google.appengine.api import urlfetch
import StringIO
import __global as glb
from copy import deepcopy
from datetime import datetime
from common.utils import util

qt = lambda (s):quote(s,'~')

CHARACTER_LIMIT = 140

# A singleton representing a lazily instantiated FileCache.
DEFAULT_CACHE = object()

def solve_time(s):
    index = s.index('+')
    zone = s[index:index+5]
    s = s.replace(zone,'')
    fmt = '%a %b %d %H:%M:%S  %Y'
    fmt2 = '%Y-%m-%d %H:%M:%S'
    return datetime.strptime(s, fmt).strftime(fmt2) + zone

def get_oauth_url():
    signature_method_hmac_sha1 = oauth.SignatureMethod_HMAC_SHA1()
    oauth_consumer = oauth.Consumer(key = CONSUMER_KEY, secret = CONSUMER_SECRET)
    oauth_client = oauth.Client(oauth_consumer)
    resp, content = oauth_client.request(REQUEST_TOKEN_URL, 'POST', 
                                         body = 'oauth_callback=oob')
    
    if resp['status'] != '200':
        return
    else:
        request_token = dict(parse_qsl(content))
        url = '%s?oauth_token=%s' % (AUTHORIZE_URL, request_token['oauth_token'])
        info = TwitterOAuthInfo(oauth_token = request_token['oauth_token'], 
                                oauth_token_secret = request_token['oauth_token_secret'])
        info.put()
        return url
                         
def get_access_token(pincode):
    info = TwitterOAuthInfo.gql('ORDER BY Time DESC').get()
    oauth_token = info.oauth_token
    oauth_token_secret = info.oauth_token_secret
    info.delete()
    token = oauth.Token(oauth_token, oauth_token_secret)
    token.set_verifier(pincode)
    oauth_consumer = oauth.Consumer(key = CONSUMER_KEY, secret = CONSUMER_SECRET)
    oauth_client = oauth.Client(oauth_consumer, token)
    resp, content = oauth_client.request(ACCESS_TOKEN_URL,
                                         method = 'POST',
                                         body = 'oauth_callback=oob&oauth_verifier=%s' % \
                                           pincode)
    access_token = dict(parse_qsl(content))
    if resp['status'] != '200':
        return
    else:
        return access_token['oauth_token'], access_token['oauth_token_secret']

def qs2dict(s): 
    dic = {} 
    for param in s.split('&'): 
        (key, value) = param.split('=') 
        dic[key] = value 
    return dic

def dict2qs(dic): 
    return ', '.join(['%s="%s"' % (key, value) for key, value in dic.iteritems()])

class TwitterError(Exception):
    '''Base class for Twitter errors'''

    @property
    def message(self):
        '''Returns the first argument used to construct this error.'''
        return self.args[0]

class API(object):
    
    def __init__(self, access_token, access_token_secret):
        self._access_token = access_token
        self._access_token_secret = access_token_secret
        self._api = _twitter.Api(CONSUMER_KEY, 
                                 CONSUMER_SECRET,
                                 access_token,
                                 access_token_secret)
        
    def __getattr__(self, attr):
        
        def wrap(**kw):
            if kw.has_key('_type'):
                _type = kw['_type'].upper()
            else:
                _type = 'REST'
            if _type == 'REST':
                url = REST_BASE_URL % attr.replace('__', '/')
            elif _type == 'STREAM':
                url = STREAM_BASE_URL % attr.replace('__','/')
            if kw.has_key('method'):
                method = kw['method'].upper()
                del kw['method']
            else:
                method = 'GET'
            logging.info(str(url)+str(kw))
            logging.info(method)
            if method == 'POST':
                res = self._api.FetchUrl(url, post_data = kw)
            elif method == 'GET':
                res = self._api.FetchUrl(url, parameters = kw)
            res = load(StringIO.StringIO(res))
            if isinstance(res, list):
                res = {'result': res}
            return res
        
        return wrap

class TwitterOAuthInfo(db.Model):
    oauth_token = db.StringProperty()
    oauth_token_secret = db.StringProperty()
    Time = db.DateTimeProperty(auto_now_add = True)
            
class Twitter(object):
    
    def __init__(self, access_token, access_token_secret):
        self._access_token = access_token
        self._access_token_secret = access_token_secret
        self.api = API(access_token, access_token_secret)
    
    def get_screen_name(self):
        res = self.api.account__settings()
        logging.info(str(res))
        res = res["screen_name"]
        return res
    
    def has_expired(self):
        res = self.api.user__show(screen_name = self.get_screen_name())
        if 'errors' in res:
            for i in res['errors']:
                if i['code']==89:
                    return True
        return False
    
    def expire_in(self):
        return 0
    
    def parse_user(self, user):
        res = deepcopy(glb.User)
        logging.info(user)
        res['id'] = user['screen_name']
        res['screen_name'] = user['name']
        res['user_name'] = user['screen_name']
        res['website'] = user['url'] if user['url'] else ''
        res['follower_count'] = user['followers_count']
        res['follow_count'] = user['friends_count']
        res['verified'] = user['verified']
        res['profile_image']['small_url'] = user['profile_image_url']
        res['profile_image']['large_url'] = user['profile_image_url']
        res['description'] = user['description']
        res['location'] = user['location']
        return res
    
    def user(self, **kw):
        key = util.make_key('user_twitter', kw)
        data = util.get_cache_data(key)
        if data:
            print 'YES'
            return data
        
        ID = kw['ID'] if kw.has_key('ID') else ''
        res = self.api.users__show(screen_name = ID)
        logging.info(str(res))
        
        response = self.parse_user(res)
        util.set_cache_data(key, response)
        return response
    
    def parse_status(self, status):
        #[begin, end, type, extra]
        entities = status['entities']
        text = status['text']
        res = deepcopy(glb.StatusText)
        result = []
        tmp = []
        
        if entities.has_key('hash_tags'):
            for hash_tag in entities['hash_tags']:
                tmp += [[hash_tag['indices'][0], hash_tag['indices'][1], 'trend', hash_tag['text']]]
        if entities.has_key('urls'):
            for url in entities['urls']:
                tmp += [[url['indices'][0], url['indices'][1], 'url', url['expanded_url']]]
        if entities.has_key('media'):
            for url in entities['media']:
                tmp += [[url['indices'][0], url['indices'][1], 'url', url['expanded_url']]]        
        if entities.has_key('user_mentions'):
            for user in entities['user_mentions']:
                tmp += [[user['indices'][0],user['indices'][1], 'mention', user['screen_name']]]
        tmp.sort()
        
        prev = 0
        for data in tmp:
            d = {}
            t = text[prev:data[0]]
            if t:
                result += [{'text': t, 'texttype': 'normal', 'extra':''}]
            result += [{'text': text[data[0]:data[1]], 'texttype': data[2], 'extra': data[3]}]
            prev = data[1] + 1
        if prev<len(text):
            result += [{'text': text[prev:len(text)], 'texttype': 'normal', 'extra':''}]
        res['text'] = text
        res['blocks'] = result
        
        return res
    
    def parse_statuses(self, statuses):
        response = []
        for status in statuses:
            s = deepcopy(glb.Status)
            s['id'] = status['id_str']
            s['text'] = self.parse_status(status)
            s['time'] = solve_time(status['created_at'])
            s['user'] = self.parse_user(status['user'])
            entities = status['entities']
            if entities.has_key('media'):
                for i in entities['media']:
                    if i['type'] == 'photo':
                        s['image']['small_url'] = i['media_url_https']                        
                        s['image']['large_url'] = i['media_url_https']
            s['retweet_count'] = status['retweet_count']
            s['favor_count'] = status['favorite_count']
            response.append(s)  
        return response
    
    def public_timeline(self, **kw):
        limit = int(kw['limit']) if 'limit' in kw else 100
        res = self.api.statuses__home_timeline(count = limit)
        res = res['result']
        response = {}
        response['result'] = self.parse_statuses(res)
        
        return response
    
    def timeline(self, **kw):
        ID = kw['ID'] if 'ID' in kw else ''
        limit = int(kw['limit']) if kw.has_key('limit') else 100
        
        res = self.api.statuses__user_timeline(screen_name = ID, count = limit)
        res = res['result']
        response = self.parse_statuses(res)
        
        logging.info(str(response))
        return {'result':response}
    
    def friends(self, **kw):
        ID = kw['ID'] if kw.has_key('ID') else ''
        cursor = kw['cursor'] if 'cursor' in kw else -1
        res = self.api.friends__list(screen_name = ID,cursor=cursor)
        logging.info(res)
        res = res['users']
        
        response = []
        for i in res:
            response.append(self.parse_user(i))
            logging.info(i)
            
        return {'result':response}
            
    def followers(self, **kw):
        ID = kw['ID'] if kw.has_key('ID') else ''
        cursor = kw['cursor'] if 'cursor' in kw else -1
        res = self.api.followers__list(screen_name = ID,cursor=cursor)
        res = res['users']
        
        response = []
        for i in res:
            response.append(self.parse_user(i))
        return {'result':response}