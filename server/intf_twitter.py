import webapp2
from common.open.twitter import *
import urllib
from common.utils import util
from google.appengine.api import urlfetch, memcache
from google.appengine.ext import db
import common.open.httplib2
import logging

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        tw = Twitter(self.request.get('oauth_token'),self.request.get('oauth_secret'))
        self.response.write(tw.api.statuses__user_timeline().decode('GB2312'))

class CallbackHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        
        pincode = self.request.get('pincode')
        param = get_access_token(pincode)
        self.response.write(param)
        
        tw = Twitter(param[0], param[1])
        req = {}
        req['account_type'] = 'twitter'
        req['account_name'] = tw.get_screen_name()
        req['access_token'] = param[0]
        req['access_secret'] = param[1]
        req['expire_in'] = 0
        
        util.redirect_to_login(self, req)
    
class OAuthHandler(webapp2.RequestHandler):
    
    def get(self):
        self.redirect(get_oauth_url())
        
    
app = webapp2.WSGIApplication([
    ('/intf/twitter/', MainHandler), 
    ('/intf/twitter/callback',CallbackHandler),
    ('/intf/twitter/oauth', OAuthHandler)
], debug=True)
