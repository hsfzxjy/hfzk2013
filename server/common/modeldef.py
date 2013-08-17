from google.appengine.ext import db
import open.sina
import open.facebook
import open.twitter

class User(db.Model):
    '''The user model definition'''
    
    ID = db.StringProperty()
    
class SNSAccount(db.Model):
    '''The SNS Account model definition'''
    
    owner = db.ReferenceProperty(User, collection_name = 'accounts')
    account_type = db.StringProperty(choices = ('sina','facebook','twitter'))
    access_secret = db.StringProperty()
    account_name = db.StringProperty()
    access_token = db.StringProperty()
    expire_in = db.IntegerProperty()