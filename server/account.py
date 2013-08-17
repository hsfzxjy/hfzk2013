import webapp2
from google.appengine.ext import db
from common.utils import util, code
from common import userop, modeldef
from common.open import sina,twitter,facebook

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        pass
    
class QueryHandler(webapp2.RequestHandler):
    
    def get(self):
        """Params
           account_type, account_name"""
        try:
            self.response.headers['Content-Type'] = 'text/plain'
            account_type = self.request.get('account_type')
            account_name = self.request.get('account_name')
            
            req = {'account_type': account_type, 
                   'account_name': account_name}
            account = userop.get_account(req)
            
            res = {'ID':''}
            if account:
                res['ID'] = str(userop.get_user(account).ID)
            else:
                res['ID'] = 'None'
        except:
            res = util.Error_Bad_Request
            
        self.response.write(code.object_to_xml(res).toxml())        
    
class ExpiredHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        account_type = self.request.get('account_type')
        account_name = self.request.get('account_name')
        req = {'account_type': account_type, 
                'account_name': account_name}        
        account = userop.get_account(req)
        
        if account:
            has_expired = getattr(self, '_do_%s' % account_type)(account)
            res = {'result': has_expired}
        else:
            res = util.Error_Bad_Request
        
        self.response.write(code.object_to_xml(res).toxml())
    
    def _do_sina(self, account):
        return sina.Sina(account.access_token).has_expired()
    
    def _do_twitter(self, account):
        return twitter.Twitter(account.access_token,account.access_secret).has_expired()
    
    def _do_facebook(self, account):
        return facebook.Facebook(account.access_token).has_expired()
    
app = webapp2.WSGIApplication([
    ('/account/', MainHandler),
    ('/account/query', QueryHandler),
    ('/account/expired', ExpiredHandler)
], debug=True)