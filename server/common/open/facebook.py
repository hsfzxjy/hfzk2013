CLIENT_ID = '184205411745570'
CLIENT_SECRET = '9a560fb2dd7fb4ff14dfae9ea22c47ac'
AUTHORIZE_URL = 'https://www.facebook.com/dialog/oauth/?client_id=%s&client_secret=%s&scope=%s&display=popup&redirect_uri=%s&force=True'
ACCESS_TOKEN_URL = 'https://graph.facebook.com/oauth/access_token?client_id=%s&client_secret=%s&redirect_uri=%s&code=%s'
REDIRECT_URI = 'https://hfzkdebug.appspot.com/intf/facebook/callback'
REDIRECT_URI2 = 'https://hfzkdebug.appspot.com/intf/facebook/callback2'
BASE_URL = 'https://graph.facebook.com/%s?access_token=%s'
PERMISSION = 'email,user_actions.books,user_actions.video,user_education_history,user_groups,user_likes,user_photos,user_relationships,user_subscriptions,user_work_history,publish_actions,user_actions.music,user_activities,user_events,user_hometown,user_location,user_questions,user_videos,user_about_me,user_actions.news,user_birthday,user_games_activity,user_interests,user_notes,user_relationship_details,user_status,user_website,friends_about_me,friends_actions.news,friends_birthday,friends_games_activity,friends_interests,friends_notes,friends_relationship_details,friends_status,friends_website,friends_actions.books,friends_actions.video,friends_education_history,friends_groups,friends_likes,friends_photos,friends_relationships,friends_subscriptions,friends_work_history,friends_actions.music,friends_activities,friends_events,friends_hometown,friends_location,friends_questions,friends_religion_politics,friends_videos,export_stream,manage_notifications,publish_stream,read_mailbox,read_stream,create_event,friends_online_presence,manage_pages,read_friendlists,read_page_mailboxes,rsvp_event,status_update,xmpp_login,create_note,manage_friendlists,read_insights,read_requests,share_item,user_online_presence'

import urllib
from urllib import quote
from google.appengine.api import urlfetch
from common.utils import code,util
import logging
import __global as glb
from copy import deepcopy
from common import sep_facebook as sf

do_quote = lambda(li):tuple([str(quote(i)) for i in li])
make_oauth_url = lambda:AUTHORIZE_URL%do_quote([CLIENT_ID, CLIENT_SECRET, PERMISSION, REDIRECT_URI])
make_access_token_url = lambda (code):ACCESS_TOKEN_URL%do_quote([CLIENT_ID, CLIENT_SECRET, REDIRECT_URI, code])

user_temp = []

def solve_time(s):
    s = s.replace('T',' ')

def solve_gender(s):
    if not s:
        return 'n'
    elif s == 'male':
        return 'm'
    elif s == 'female':
        return 'f'
    
class API(object):
    
    def __init__(self, access_token):
        self.__access_token = access_token
        
    def __getattr__(self, attr):
        
        def wrap(**kw):
            at = attr[1:] if attr[0] == '_' else attr
            url = BASE_URL % (at.replace('__','/'), self.__access_token)
            
            method = 'GET'
            param = ''
            if kw:
                if 'method' in kw:
                    method = kw['method']
                    del kw['method']
                else:
                    method = 'GET'
                param = '&'.join(['%s=%s'%(i,quote(j,safe='()')) for i,j in kw.iteritems()])
               
            if method == 'GET':
                if param:
                    url += '&'+param
                result = urlfetch.fetch(url).content
            elif method == 'POST':
                result = urlfetch.fetch(url,payload=param,method=urlfetch.POST).content
            result = code.json_to_object(result)
            return result
        
        return wrap

class Facebook(object):
    
    def __init__(self, access_token):
        self.__access_token = access_token
        self.api = API(access_token)
        
    def expire_in(self):
        return 8
    
    def has_expired(self):
        res = self.api.me()
        if 'error' in res:
            error = res['error']
            if error['code'] == 190 and error['error_subcode'] == 463:
                return True
        return False
    
    def update_status(self, **kw):
        message = kw['message'] if 'message' in kw else ''
        if not message:
            return util.Error_Bad_Request
        res = self.api.me__feed(method='POST',message=message)
        
    
    def parse_statuses(self, res):
        response = []
        for status in res:
            s = deepcopy(glb.Status)
            s['id'] = status['id']
            s['text'] = sf.sep(status)
            if 'picture' in status:
                s['image']['large_url'] = status['picture']
                s['image']['small_url'] = status['picture']
            s['application'] = status['application']['name'] if \
                status.has_key('application') else ''
            s['user'] = self.user(ID = status['from']['id'])
            s['time'] = solve_time(status['created_time'])
            if status.has_key('comments'):
                for c in status['comments']['data']:
                    com = deepcopy(glb.Comment)
                    com['text'] = sf.sep(c)
                    com['time'] = solve_time(c['created_time'])
                    com['id'] = c['id']
                    com['user'] = self.user(ID = c['from']['id'])
                    s['comments'].append(com)
            s['good_count'] = status['like']['count'] if status.has_key('like') else 0
            s['comment_count'] = len(s['comments'])
            response.append(s)
        
        return response
    
    def timeline(self, **kw):
        limit = int(kw['limit']) if kw.has_key('limit') else 100
        ID = kw['ID'] if 'ID' in kw else ''
        if not ID:
            res = self.api.me(fields = 'feed.limit(%s)'%str(limit))['feed']['data']
        else:
            res = getattr(self.api, '_'+ID)(fields = 'feed.limit(%s)'%str(limit))['feed']['data']
        response = self.parse_statuses(res)
        return {'result': response}
    
    def public_timeline(self, **kw):
        limit = int(kw['limit']) if 'limit' in kw else 100
        res = self.api.me(fields = 'home.limit(%s)'%str(limit))
        logging.info(res)
        res = res['home']['data']
        response = self.parse_statuses(res)
        return {'result': response}
    
    def user(self, **kw):
        ID = kw['ID'] if kw.has_key('ID') else ''
        for user in user_temp:
            if user['id'] == ID:
                return user
        res = getattr(self.api, '_'+ID)()
        response = deepcopy(glb.User)
        response['id'] = res['id']
        response['screen_name'] = res['name']
        response['user_name'] = res['username'] if res.has_key('username') else ''
        response['gender'] = res['gender'] if 'gender' in res else ''
        response['gender'] = solve_gender(response['gender'])
        response['website'] = res['website'] if res.has_key('website') else ''
        response['profile_image']['large_url'] = \
            getattr(self.api, '_'+ID)(fields = 'picture')['picture']['data']['url']
        response['profile_image']['small_url'] = \
                    response['profile_image']['large_url']
        response['description'] = res['quotes'] if 'quotes' in res else ''
        user_temp.append(response)
        
        return response
    
    def friends(self, **kw):
        func = '_'+kw['ID'] if kw.has_key('ID') else 'me'
        res = getattr(self.api, func)(fields = 'friends')['friends']['data']
        logging.info(str(res))
        
        response = []
        for data in res:
            logging.info(data['id'])
            response.append(self.user(ID = data['id']))
            
        return response