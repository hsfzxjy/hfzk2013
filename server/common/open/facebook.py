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
from common.utils import code

do_quote = lambda(li):tuple([str(quote(i)) for i in li])
make_oauth_url = lambda:AUTHORIZE_URL%do_quote([CLIENT_ID, CLIENT_SECRET, PERMISSION, REDIRECT_URI])
make_access_token_url = lambda (code):ACCESS_TOKEN_URL%do_quote([CLIENT_ID, CLIENT_SECRET, REDIRECT_URI, code])

class API(object):
    
    def __init__(self, access_token):
        self.__access_token = access_token
        
    def __getattr__(self, attr):
        
        def wrap(**kw):
            url = BASE_URL % (attr.replace('__','/'), self.__access_token)+ \
                urllib.urlencode(kw)
            result = urlfetch.fetch(url).content
            result = code.json_to_object(result)
            return result
        
        return wrap

class Facebook(object):
    
    def __init__(self, access_token):
        self.__access_token = access_token
        self.api = API(access_token)
        
    def expire_in(self):
        return 8