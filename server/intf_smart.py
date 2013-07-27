import webapp2
from common.open import sina, twitter, facebook
from common.utils import code

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
        result = getattr(self,'_do_%s'%account_type)(req)
        
        _type = self.request.get('type') if self.request.get('type') else 'xml'
        if _type.lower() == 'xml':
            self.response.write(code.object_to_xml(result).toxml())
        else:
            self.response.write(code.object_to_json(result))
        
    def _do_facebook(self, req):
        fb = facebook.Facebook(self._access_token)
        return getattr(fb, self._category)(**req)
    
    def _do_twitter(self, req):
        tw = twitter.Twitter(self._access_token, self._access_secret)
        return getattr(tw, self._category)(**req)
    
app = webapp2.WSGIApplication([
    ('/intf/smart', MainHandler)
], debug=True)
