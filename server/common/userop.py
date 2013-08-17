from google.appengine.ext import db
import modeldef
import logging

class UserError(StandardError):
    
    def __init__(self, msg = ''):
        self.__msg = msg
        
    def __str__(self):
        return self.__msg

def get_data(param):
    if isinstance(param, int) or isinstance(param, str) or isinstance(param, unicode):
        return get_data_from_ID(int(param))
    elif isinstance(param, modeldef.User):
        return get_data_from_user(param)
    elif isinstance(param, modeldef.SNSAccount):
        return {"account_type":param.account_type, 
          "account_name":param.account_name,
          "access_token":param.access_token,
          "expire_in":param.expire_in,
          "access_secret":param.access_secret}
    else:
        return None
    
def get_user(param):
    result = None
    if isinstance(param, int) or isinstance(param, str) or isinstance(param, unicode):
        result = get_user_from_ID(int(param))
    elif isinstance(param, dict):
        result = get_user_from_account(param['account_type'], param['account_name'])
    elif isinstance(param, modeldef.SNSAccount):
        result = get_user_from_account(param.account_type, param.account_name)
    else:
        return None
    return result
    
def get_data_from_ID(ID):
    user = get_user_from_ID(ID)
    if not user:
        raise UserError("The ID is invalid!")
    return get_data_from_user(user)
    
def get_data_from_user(user):
    result = {"ID" : user.ID}
    result["accounts"] = []
    for u in user.accounts:
        result["accounts"].append(
              {"account_type":u.account_type, 
              "account_name":u.account_name,
              "access_token":u.access_token,
              "expire_in":u.expire_in,
              "access_secret":u.access_secret})
    return result

def get_user_from_ID(ID):
    try:
        gql = modeldef.User.gql("WHERE ID = '%s'" % str(ID))
    except Exception:
        gql = None
        raise UserError('User query error.')
    result = gql.get()
    return result

def get_user_from_account(_type, _name):
    data = {'account_type':_type, 'account_name':_name}
    acc = get_account(data)
    if not acc:
        return None
    return acc.owner

def get_next_ID():
    gql = modeldef.User.gql("ORDER BY ID DESC")
    user = gql.get()
    if not user:
        return 1
    else:
        return user.ID + 1

def insert_data(data, ID = '', user = None):
    """
        e.g. insert_data(0001, {"user_name":13434312012, "access_token":XXX,
          "sns":"sina"})
    """
    if ID:
        _user = get_user_from_ID(int(ID))
        user = _user
    if not user:
        raise UserError("ID %s does not exist!" % str(ID))
    if get_user_from_account(data["account_type"], data["account_name"]):
        return True
    acc = modeldef.SNSAccount(owner = user, 
                        account_name = data["account_name"],
                        account_type = data["account_type"],
                        access_token = data["access_token"],
                        expire_in = int(data["expire_in"]))
    if data.has_key("access_secret"):
        acc.access_secret = data["access_secret"]
    acc.put()
   
def modify_data(param, **kw):
    account = get_account(param)
    if not account:
        return False
    for k,v in kw.iteritems():
        setattr(account, k, v)
    account.put()
    return True
    
def login(data):
    user = get_user_from_account(data["account_type"], data["account_name"])
    if user:
        modify_data(data, access_token = data["access_token"])
        return user.ID
    _ID = get_next_ID()
    s = modeldef.User(ID = _ID)
    s.put()
    insert_data(data, user = s)
    return s.ID

def get_account(data):
    if isinstance(data, dict):
        try:
            gql = modeldef.SNSAccount.gql("WHERE account_name = '%s' AND account_type = '%s'" %(
            data['account_name'], data['account_type']))
            acc = gql.get()
        except:
            acc = None
    elif isinstance(data, modeldef.SNSAccount):
        acc = data
    else:
        return None   
    return acc