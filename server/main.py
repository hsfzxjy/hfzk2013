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

html = '''<html>
<head><title>Welcome to HSFZZK Debug!</title></head>
<body>
<center><h1>Welcome to HSFZZK Debug!</h1><br/>
<a href="/intf/sina/oauth"><img src="img/wb.png"></a><br/>
<a href="/intf/twitter/oauth"><img src="img/tw.png"></a><br/>
<a href="/intf/facebook/oauth"><img src="img/fb.png"></a><br/></center>
</body></html>'''

class MainHandler(webapp2.RequestHandler):
    def get(self):
        self.response.write(html)
        
app = webapp2.WSGIApplication([
    ('/', MainHandler)
], debug=True)
