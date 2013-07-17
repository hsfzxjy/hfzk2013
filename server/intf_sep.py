import webapp2
from common.utils.code import object_to_xml
import logging
import re

REG_SINA = ur'(@[\u0030-\u0039\u0041-\u005a\u0061-\u007a\u4e00-\u9fa5-_]+|#[^#]+#|http\://t\.cn/[a-zA-Z0-9]{7}|\[.+\])'
REG_TWITTER = ur''
reg = {'sina':REG_SINA, 'twitter':REG_TWITTER}

def make_element(text, ty):
    return {'text':text,'texttype':ty}

class MainHandler(webapp2.RequestHandler):
    
    def get(self):
        self.response.headers['Content-Type'] = 'text/plain'
        self._text = self.request.get('text')
        self._sns = self.request.get('sns')
        self._exp = reg[self._sns]
        res = {'result': self._work()}
        self.response.write(object_to_xml(res).toxml())
        
    def _work(self):
        s = unicode(self._text)
        l = re.findall(self._exp,s,re.UNICODE)
        offset = 0
        res=[]
        for i in l:
            a=s.find(i)
            if a != offset:
                res.append(make_element(s[offset:a],'normal'))
            if re.match(ur'\[.+\]', i, re.UNICODE) and self._sns == 'sina':
                res.append(make_element(i,'emotion'))
            else:
                res.append(make_element(i,'link'))
            offset = a + len(i)
        if s[offset:]:
            res.append(make_element(s[offset:],'normal'))
        return res        
        
    
app = webapp2.WSGIApplication([
    ('/intf/sep', MainHandler)
], debug=True)