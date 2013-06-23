from google.appengine.ext import db
import modeldef

class UserError(StandardError):
    
    def __init__(self, msg = ''):
        self.__msg = msg
        
    def __str__(self):
        return self.__msg

def get_data_from_ID(ID):
    user = get_user_from_ID(ID)
    if not user:
        raise UserError("The ID is invalid!")
    return get_data_from_user(user)
    
def get_data_from_user(user):
    result = {"ID" : user.ID}
    result["accounts"] = [
          {"account_type":u.account_type, 
          "account_name":u.account_name,
          "access_token":u.access_token} for u in user.accounts]
    return result

def get_user_from_ID(ID):
    try:
        gql = modeldef.User.gql("WHERE ID = %s" % str(ID))
    except Exception:
        gql = None
        raise UserError('User query error.')
    return gql.get()

def get_user_from_account(_type, _name):
    try:
        gql = modeldef.SNSAccount.gql('WHERE account_name = \'%s\' AND account_type = \'%s\'' % (
                _name, _type))
    except Exception:
        raise UserError('User query Error.')
    s = gql.get()
    if not s: 
        return None
    else:
        return s.owner

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
        _user = get_user_from_ID(ID)
        user = None
    if not user:
        raise UserError("ID %s does not exist!" % str(ID))
    if get_user_from_account(data["account_type"], data["account_name"]):
        return True
    modeldef.SNSAccount(owner = user, 
                        account_name = data["account_name"],
                        account_type = data["account_type"],
                        access_token = data["access_token"]).put()
    
def login(data):
    user = get_user_from_account(data["account_type"], data["account_name"])
    if user:
        return user.ID
    _ID = get_next_ID()
    s = modeldef.User(ID = _ID)
    s.put()
    insert_data(data, user = s)
    return s.ID