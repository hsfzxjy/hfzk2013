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
        self.response.headers['Content-Type'] = 'text/plain'
        arg = util.login_data_decode(self.request)
        #ID = userop.login(arg)
        #self.response.write(ID)
        self.response.write(code.object_to_xml(arg).toxml())

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
        '''
          e.g. ID=1&method=(add|combine|delete)
        '''
        self.__ID = int(self.request.get('ID'))
        method = self.request.get('method')
        data = {}
        for i in self.request.arguments():
            if i not in ('ID', 'method'):
                data[i] = self.request.get(i)
        #try:
        res = getattr(self, '_do_%s' % method)(data)
        #except:
            #res = None
        if not res:
            res = {'error':'Somethind has been wrong!'}
        self.response.write(code.object_to_xml(res).toxml())
        
    def _do_add(self, data):
        '''
          e.g. account_type=sina&account_name=XXXXXXXX&access_token=XXXXXXXXX
            expire_in=XXXX
        '''
        res = {}
        if userop.get_account(data):
            res['OK'] = 'The account has already existed!'
            return res
        userop.insert_data(data, ID = self.__ID)
        res['OK'] = 'Add successfully!'
        return res
    
    def _do_delete(self, data):
        '''
          e.g. account_type=sina&access_name=XXXXXXXX
        '''
        res = {}
        account = userop.get_account(data)
        if account:
            account.delete()
            res['OK'] = 'Delete successfully!'
            return res
    
    def _do_combine(self, data):
        '''
          e.g. ID2=2
        '''
        user2 = userop.get_user(int(data['ID2']))
        user = userop.get_user(self.__ID)
        accounts = user2.accounts
        for acc in accounts:
            acc.owner = user
            acc.put()
        user2.delete()
        return {'OK': 'Combine successfully!'}
    
app = webapp2.WSGIApplication([
    ('/user/login', User_LoginHandler), 
    ('/user/query', User_QueryHandler),
    ('/user/operate', User_OperateHandler)
], debug=True)
