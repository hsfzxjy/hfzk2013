from google.appengine.ext import db
import webapp2
from common.utils import util, code
from common import userop, modeldef

class User_LoginHandler(webapp2.RequestHandler):

    def post(self):
        pass
    
    def get(self):
        '''
          Request e.g. 
          account_type=sina&account_name=13434312012&access_token=XXXXXXXXXXXXX
        '''
        arg = util.login_data_decode(self.request)
        ID = userop.login(arg)
        self.response.write(ID)

class User_QueryHandler(webapp2.RequestHandler):

    def get(self):
        '''
           Request e.g.
           ID=12&type=json
        '''
        self.response.headers["Content-Type"] = 'text/plain'
        ID = int(self.request.get("ID"))
        type = self.request.get("type")
        try:
            info = userop.get_data(ID)
        except userop.UserError, e:
            info = {"error" : str(e)}
        res = code.object_to_xml(info).toxml() if type == 'xml' else \
                        code.object_to_json(info)
        self.response.write(res)
    
class User_OperateHandler(webapp2.RequestHandler):
    
    def get(self):
        pass
    
app = webapp2.WSGIApplication([
    ('/user/login', User_LoginHandler), 
    ('/user/query', User_QueryHandler)
], debug=True)
