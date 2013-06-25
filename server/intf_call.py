import webapp2
from utils.code import object_to_xml
import open.sina
import logging

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers["Content-Type"] = 'text/plain'
    try:
        sns = self.request.get("sns")
        func_name = self.request.get("func_name")
        access_token = self.request.get("access_token")
        req = {}
        for i in self.request.arguments():
            if i not in ("sns", "func_name","access_token"):
                req[i] = self.request.get(i)
        response = getattr(self, "_do_%s"%sns)(func_name, access_token, req)
    except:
        response = {"_error": "Bad request!"}
        self.response.clear()
        self.response.write(object_to_xml(response).toxml())
    
    def post(self):
        """params:
           sns: (sina|twitter|facebook) *
           func_name: *
           access_token: *
           parameter: optional"""
        try:
            sns = self.request.get("sns")
            func_name = self.request.get("func_name")
            access_token = self.request.get("access_token")
            req = {}
            for i in self.request.arguments():
                if i not in ("sns", "func_name","access_token"):
                    req[i] = self.request.get(i)
            response = self.__getattr__("_do_%s"%sns)(func_name, access_token, req)
        except:
            response = {"_error": "Bad request!"}
        self.response.write(object_to_xml(response).toxml())
        
    def _do_sina(self, func_name, access_token, req):
        try:
            s = open.sina.Sina(access_token)
            response = getattr(s.api, func_name)(**req)
        except:
            response = {"_error": "Bad request!"}
        return response
    
    def _do_twitter(self, func_name, access_token, req):
        pass     #Haven't implemented
    
    def _do_facebook(self, func_name, access_token, req):
        pass     #Haven't implemented
    
app = webapp2.WSGIApplication([
    ('/intf/call', MainHandler)
], debug=True)