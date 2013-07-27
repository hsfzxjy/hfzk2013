__version__ = '1.0'

User = {'id':'',
        'screen_name':'',
        'user_name':'',
        'follow_count':0,
        'follower_count':0,
        'profile_image_url':'',
        'gender':'',
        'website':''}
Link = {'show':'',
        'url':''}
Emotion = {'text': '', 'url': ''}
Comment = {'id': '',
           'text': '',
           'user': User,
           'time': ''}
Trend = {'text': ''}

StatusText = []
RetweetStatus = {'id': '',
                 'text': '',
                 'user': '',
                 'time': '',
                 'image_url': '',
                 'favor_count': 0,
                 'retweet_count': 0,
                 'good_count': 0,
                 'comment_count': 0,
                 'comments': [],
                 'application': ''
                 }
Status = {'id': '',
          'text': '',
          'user': User,
          'time': '',
          'image_url': '',
          'favor_count': 0,
          'retweet_count': 0,
          'good_count': 0,
          'comment_count': 0,
          'comments':[],
          'application': '',
          'has_retweet': False,
          'retweet': RetweetStatus
          }
Friends = {'follows' :[],
           'followers' : [],
           'friends' : []
           }