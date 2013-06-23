from google.appengine.ext import db

class User(db.Model):
    '''The user model definition'''
    
    ID = db.IntegerProperty()
    
class SNSAccount(db.Model):
    '''The SNS Account model definition'''
    
    owner = db.ReferenceProperty(User, collection_name = 'accounts')
    account_type = db.StringProperty(choices = ('sina','facebook','twitter'))
    account_name = db.StringProperty()
    access_token = db.StringProperty()
    