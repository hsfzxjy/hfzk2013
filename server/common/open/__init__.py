__version__ ='1.0.0'

class User(object):

    def __init__(self, fb='', tw='', si=''):
        self.accounts = {}
        self.access_tokens = {}
        self.accounts['facebook'] = fb
        self.accounts['twitter'] = tw
        self.accounts['sina'] = si

