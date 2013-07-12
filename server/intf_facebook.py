import webapp2
from common.open.facebook import *
import urllib
from common.utils import util, code
from google.appengine.api import urlfetch, memcache
import logging

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        pass

class CallbackHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        data = {}
        res = urlfetch.fetch(make_access_token_url(self.request.get('code')))
        res = util.urldecode(res.content)
        data['access_token'] = res['access_token']
        data['expire_in'] = int(res['expires'])
        data['account_type'] = 'facebook'
        res = code.json_to_object(urlfetch.fetch(BASE_URL%('me', data['access_token'])).content)
        data['account_name'] = str(res['id'])
        util.redirect_to_login(self, data)
    
class OAuthHandler(webapp2.RequestHandler):
    
    def get(self):
        self.redirect(make_oauth_url())
    
app = webapp2.WSGIApplication([
    ('/intf/facebook/', MainHandler), 
    ('/intf/facebook/callback',CallbackHandler),
    ('/intf/facebook/oauth', OAuthHandler)
], debug=True)