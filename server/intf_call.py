import webapp2
from common.utils.code import object_to_xml
from common.utils import util
from common.open import sina, facebook, twitter
import logging

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        """params:
           sns: (sina|twitter|facebook) *
           func_name: *
           access_token: *
           parameter: optional"""
        self.response.headers["Content-Type"] = 'text/plain'
        try:
            sns = self.request.get("sns")
            func_name = self.request.get("func_name")
            access_token = self.request.get("access_token")
            access_secret = self.request.get("access_secret")
            req = {}
            for i in self.request.arguments():
                if i not in ("sns", "func_name","access_token","access_secret"):
                    req[i] = self.request.get(i)
            account = getattr(self, "_do_%s"%sns)(func_name, access_token, access_secret, req)
            if not account.has_expired():
                response = getattr(account.api, func_name)(**req)
            else:
                response = util.Error_Expired
        except util.DeadlineErrors:
            response = util.Error_Server_Busy
        except:
            response = util.Error_Bad_Request
        self.response.write(object_to_xml(response).toxml())
    
    def post(self):
        """params:
           sns: (sina|twitter|facebook) *
           func_name: *
           access_token: *
           parameter: optional"""
        self.response.headers["Content-Type"] = 'text/plain'
        try:
            sns = self.request.get("sns")
            func_name = self.request.get("func_name")
            access_token = self.request.get("access_token")
            access_secret = self.request.get("access_secret")
            req = {}
            for i in self.request.arguments():
                if i not in ("sns", "func_name","access_token","access_secret"):
                    req[i] = self.request.get(i)
            account = getattr(self, "_do_%s"%sns)(func_name, access_token, access_secret, req)
            if not account.has_expired():
                response = getattr(account.api, func_name)(**req)
            else:
                response = util.Error_Expired
        except util.DeadlineErrors:
            response = util.Error_Server_Busy
        except:
            response = util.Error_Bad_Request
        self.response.write(object_to_xml(response).toxml())
        
    def _do_sina(self, func_name, access_token, access_secret, req):
        return sina.Sina(access_token)
    
    def _do_twitter(self, func_name, access_token, access_secret, req):
        return twitter.Twitter(access_token, access_secret)
    
    def _do_facebook(self, func_name, access_token, access_secret, req):
        return facebook.Facebook(access_token)
    
app = webapp2.WSGIApplication([
    ('/intf/call', MainHandler)
], debug=True)