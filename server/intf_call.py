import webapp2
from common.utils.code import object_to_xml
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
    #try:
        sns = self.request.get("sns")
        func_name = self.request.get("func_name")
        access_token = self.request.get("access_token")
        access_secret = self.request.get("access_secret")
        req = {}
        for i in self.request.arguments():
            if i not in ("sns", "func_name","access_token","access_secret"):
                req[i] = self.request.get(i)
        response = getattr(self, "_do_%s"%sns)(func_name, access_token, access_secret, req)
   # except:
    #    response = {"_error": "Bad request!"}
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
            response = getattr(self, "_do_%s"%sns)(func_name, access_token, access_secret, req)
        except:
            response = {"_error": "Bad request!"}
        self.response.write(object_to_xml(response).toxml())
        
    def _do_sina(self, func_name, access_token, access_secret, req):
        try:
            s = sina.Sina(access_token)
            response = getattr(s.api, func_name)(**req)
        except:
            response = {"_error": "Bad request!"}
        return response
    
    def _do_twitter(self, func_name, access_token, access_secret, req):
        try:
            tw = twitter.Twitter(access_token, access_secret)
            response = getattr(tw.api, func_name)(**req)
        except:
            response = {"_error":"Bad request!"}
        return response
    
    def _do_facebook(self, func_name, access_token, access_secret, req):
    #try:
        f = facebook.Facebook(access_token)
        response = getattr(f.api, func_name)(**req)
    #except:
     #   response = {"_error": "Bad request!"}
        return response
    
app = webapp2.WSGIApplication([
    ('/intf/call', MainHandler)
], debug=True)