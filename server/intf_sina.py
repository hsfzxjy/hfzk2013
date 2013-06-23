#!/usr/bin/env python
#
# Copyright 2007 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
import webapp2
import open.sina
import urllib
from utils import util

class MainHandler(webapp2.RequestHandler):
    def get(self):
        oauth = open.sina.OAuth()
        self.redirect(oauth.get_authorize_url())

class CallBackHandler(webapp2.RequestHandler):
    def get(self):
        code=self.request.get('code')
        self.response.write(code+'\n')
        oauth = open.sina.OAuth()
        access_token = oauth.get_access_token(code)
        s = open.sina.Sina(access_token)
        req = {}
        self.response.write(s.api.account__get_uid())
        req["account_name"] = s.api.account__get_uid()["uid"]
        req["account_type"] = "sina"
        req["access_token"] = access_token
        util.redirect_to_login(self, req)
        
app = webapp2.WSGIApplication([
    ('/intf/sina/', MainHandler), ('/intf/sina/callback',CallBackHandler)
], debug=True)
