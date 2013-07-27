from google.appengine.ext import db
import webapp2
from common.utils import util, code
from common import userop, modeldef

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        pass

class User_ModifyAccountHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        account_type = self.request.get('account_type')
        account_name = self.request.get('account_name')
        access_token = self.request.get('access_token')
        access_secret = self.request.get('access_secret')
        if not access_secret:
            access_secret = ''
        req = locals()
        del req['self']
        userop.modify_data(req, access_token = access_token, access_secret = access_secret)

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
        res = {}
        if arg["account_name"]:
            res = arg
        else:
            res["_error"] = 'Server busy!'
        self.response.write(code.object_to_xml(res).toxml())

class User_QueryAccountHandler(webapp2.RequestHandler):
    
    def get(self):
        """Params
           account_type, account_name"""
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
            
        self.response.write(code.object_to_xml(res).toxml())

class User_QueryUserHandler(webapp2.RequestHandler):

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
            info = {"_error" : str(e)}
        res = code.object_to_xml(info).toxml() if type == 'xml' else \
                        code.object_to_json(info)
        self.response.write(res)
    
class User_OperateHandler(webapp2.RequestHandler):
    
    def get(self):
        '''
          e.g. ID=1&method=(add|combine|delete|create)
        '''
        self.response.headers['Content-Type'] = 'text/plain'
        method = self.request.get('method')
        if method != 'create':
            self.__ID = int(self.request.get('ID'))
        data = {}
        for i in self.request.arguments():
            if i not in ('ID', 'method'):
                data[i] = self.request.get(i)
        #try:
        res = getattr(self, '_do_%s' % method)(data)
        #except:
            #res = None
        if not res:
            res = {'_error':'Somethind has been wrong!'}
        self.response.write(code.object_to_xml(res).toxml())
        
    def _do_create(self, data):
        user = modeldef.User()
        user.put()
        user.ID = str(user.key().id())
        user.put()
        return {'ID': user.ID}
        
    def _do_add(self, data):
        '''
          e.g. account_type=sina&account_name=XXXXXXXX&access_token=XXXXXXXXX
            expire_in=XXXX
        '''
        res = {}
        if userop.get_account(data):
            res['_error'] = 'The account has already existed!'
            return res
        userop.insert_data(data, ID = self.__ID)
        res['OK'] = 'Add successfully!'
        res = data
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
        else:
            res['_error'] = 'The account is not existed!'
        return res
    
    def _do_combine(self, data):
        '''
          e.g. ID2=2
        '''
        user2 = userop.get_user(int(data['ID2']))
        user = userop.get_user(self.__ID)
        res = {}
        if user2:
            accounts = user2.accounts
            for acc in accounts:
                acc.owner = user
                acc.put()
            user2.delete()
            res = {'OK': 'Combine successfully!'}
        else:
            res = {'_error': 'The second user is not existed!'}
        return res
    
app = webapp2.WSGIApplication([
    ('/user/login', User_LoginHandler), 
    ('/user/queryuser', User_QueryUserHandler),
    ('/user/queryaccount', User_QueryAccountHandler),
    ('/user/operate', User_OperateHandler),
    ('/user/', MainHandler),
    ('/user/modifyaccount', User_ModifyAccountHandler)
], debug=True)