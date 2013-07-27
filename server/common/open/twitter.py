# -*- coding: cp936 -*-

__version__ = '1.0'

CONSUMER_KEY = '9lY1CjPgEiQTMNXdYnEiw'
CONSUMER_SECRET = 'hIvCLhG6u37RUfjYI7wa6bHDbjjgGZx6h5S37LTwE'
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

qt = lambda (s):quote(s,'~')

CHARACTER_LIMIT = 140

# A singleton representing a lazily instantiated FileCache.
DEFAULT_CACHE = object()

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
    
    def expire_in(self):
        return 0
    
    def user(self, **kw):
        ID = kw['ID'] if kw.has_key('ID') else ''
        screen_name = kw['screen_name'] if kw.has_key('screen_name') else ''
        if ID:
            res = self.api.users__show(user_id = ID)
        else:
            res = self.api.users__show(screen_name = screen_name)
        logging.info(str(res))
        response = deepcopy(glb.User)
        response['id'] = res['id_str'] 
        response['screen_name'] = res['name']
        response['user_name'] = res['screen_name'] 
        response['website'] = res['url'] if res['url'] else ''
        response['follower_count'] = res['followers_count']
        response['follow_count'] = res['friends_count']
        response['profile_image_url'] = res['profile_image_url']
        
        return response
    
    def parse_status(self, status):
        #[begin, end, type, extra]
        entities = status['entities']
        text = status['text']
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
                tmp += [[user['indices'][0],user['indices'][1], 'at', user['name']]]
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
        return result
            
    
    def timeline(self, **kw):
        limit = int(kw['limit']) if kw.has_key('limit') else 100
        res = self.api.statuses__home_timeline(count = limit)
        res = res['result']
        response = []
        for status in res:
            s = deepcopy(glb.Status)
            s['id'] = status['id_str']
            s['text'] = self.parse_status(status)
            s['time'] = status['created_at']
            s['user'] = status['user']['screen_name']
            entities = status['entities']
            if entities.has_key('media'):
                for i in entities['media']:
                    s['image_url'] = ''
                    s['image_url'] = i['media_url_https'] if i['type'] == 'photo' else s['image_url']
            s['retweet_count'] = status['retweet_count']
            s['favor_count'] = status['favorite_count']
            response.append(s)
        
        return {'result':response}
    
    def friends(self, **kw):
        ID = kw['ID'] if kw.has_key('ID') else ''
        screen_name = kw['screen_name'] if kw.has_key('screen_name') else ''
        if ID:
            res = self.api.friends__ids(user_id = ID)
        else:
            res = self.api.friends__ids(screen_name = screen_name)
        logging.info(res)
        res = res['ids']
        
        response = deepcopy(glb.Friends)
        for i in res:
            response['follows'].append(self.user(ID = i))
            
        if ID:
            res = self.api.followers__ids(user_id = ID)
        else:
            res = self.api.followers__ids(screen_name = screen_name)
        res = res['ids']
        
        for i in res:
            response['followers'].append(self.user(ID = i))
            
        response['friends'] = [i for i in response['followers'] for j in response['follows'] if i['id']==j['id']]
            
        return response