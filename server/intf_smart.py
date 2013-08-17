import webapp2
from common.open import sina, twitter, facebook
from common.utils import util, code
from google.appengine import runtime
from google.appengine.runtime import apiproxy_errors

class MainHandler(webapp2.RequestHandler):
    def get(self):
        account_type = self.request.get('sns')
        self._category = self.request.get('category')
        self._access_token = self.request.get('access_token')
        self._access_secret = self.request.get('access_secret') if self.request.get('access_secret') else ''
        req = {}
        for i in self.request.arguments():
            if i not in ('sns', 'access_token', 'account_type', 'access_secret'):
                req[i] = self.request.get(i)
        self.response.headers['Content-Type'] = 'text/plain'
        try:
            account = getattr(self,'_do_%s'%account_type)(req)
            result = getattr(account, self._category)(**req)
        except util.DeadlineErrors:
            result = util.Error_Server_Busy
        except:
            result = util.Error_Bad_Request
            
        _type = self.request.get('type') if self.request.get('type') else 'xml'
        if _type.lower() == 'xml':
            self.response.write(code.object_to_xml(result).toxml())
        else:
            self.response.write(code.object_to_json(result))
        
    def _do_sina(self, req):
        return sina.Sina(self._access_token)
    
    def _do_facebook(self, req):
        return facebook.Facebook(self._access_token)
    
    def _do_twitter(self, req):
        return twitter.Twitter(self._access_token, self._access_secret)
    
app = webapp2.WSGIApplication([
    ('/intf/smart', MainHandler)
], debug=True)
