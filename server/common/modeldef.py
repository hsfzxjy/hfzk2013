from google.appengine.ext import db
import open.sina
import open.facebook

class User(db.Model):
    '''The user model definition'''
    
    ID = db.IntegerProperty()
    
class SNSAccount(db.Model):
    '''The SNS Account model definition'''
    
    owner = db.ReferenceProperty(User, collection_name = 'accounts')
    account_type = db.StringProperty(choices = ('sina','facebook','twitter'))
    access_secret = db.StringProperty()
    account_name = db.StringProperty()
    access_token = db.StringProperty()
    expire_in = db.IntegerProperty()
    
    def __sns(self):
        if self.account_type == 'sina':
            return open.sina.Sina(self.access_token)
        if self.account_type == 'facebook':
            return open.facebook.Facebook(self.access_token)
        
    def update(self):
        sns = self.__sns()
        self.expire_in = sns.expire_in()
        self.put()