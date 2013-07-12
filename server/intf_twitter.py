import webapp2
from common.open.twitter import *
import urllib
from common.utils import util
from google.appengine.api import urlfetch, memcache
import logging

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        tw = Twitter(self.request.get('oauth_token'),self.request.get('oauth_secret'))
        self.response.write(tw.api.statuses__user_timeline().decode('GB2312'))

class CallbackHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        self.response.write(self.request.get('oauth_token')+'\n')
        oauth_token_secret=memcache.get(self.request.get('oauth_token'),'TwitterRequestToken')
        self.response.write(oauth_token_secret+'\n')
        params = get_oauth_params({'oauth_token':self.request.get('oauth_token'),
                                   'oauth_verifier':self.request.get('oauth_verifier')},
                                  ACCESS_TOKEN_URL, oauth_token_secret)
        res = urlfetch.fetch(url=ACCESS_TOKEN_URL, headers={'Authorization': 'OAuth ' + dict2qs(params)},method=urlfetch.POST)
        dic = qs2dict(res.content)
        param = {}
        param['access_secret'] = dic['oauth_token_secret']
        param['access_token'] = dic['oauth_token']
        param['account_type'] = 'twitter'
        param['account_name'] = ''
        param['expire_in'] = 0
        #self.response.write(self.request.get('oauth_token')
        #util.redirect_to_login(self, param)
        #self.redirect('/intf/twitter/')
        tw = Twitter(param['access_token'], param['access_secret'])
        self.response.write(param['access_token']+' '+param['access_secret']+'\n')
        self.response.write(tw.api.statuses__user_timeline())        
    
class OAuthHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type']='text/plain'
        params = get_oauth_params({}, REQUEST_TOKEN_URL)
        res = urlfetch.fetch(url=REQUEST_TOKEN_URL, headers={'Authorization':'OAuth '+dict2qs(params)},method = urlfetch.POST)
        if res.status_code != 200:
            logging.error('Fetch request token error.error code:%d'%res.status_code)
            return
        dic = qs2dict(res.content)
        self.response.write(res.content)
        if dic['oauth_callback_confirmed']!='true':
            return
        memcache.set(dic['oauth_token'],dic['oauth_token_secret'],600,namespace='TwitterRequestToken')
        self.redirect('%s?oauth_token=%s' % (AUTHORIZE_URL, dic['oauth_token']))
        
    
app = webapp2.WSGIApplication([
    ('/intf/twitter/', MainHandler), 
    ('/intf/twitter/callback',CallbackHandler),
    ('/intf/twitter/oauth', OAuthHandler)
], debug=True)
