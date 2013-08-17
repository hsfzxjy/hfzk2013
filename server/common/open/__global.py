__version__ = '1.0'

Image = {'large_url': '',
         'small_url': ''}

User = {'id':'',
        'screen_name':'',
        'user_name':'',
        'follow_count':0,
        'follower_count':0,
        'profile_image':Image,
        'gender':'n',
        'location':'',
        'description':'',
        'website':'',
        'verified': False}

Link = {'show':'',
        'url':''}
Emotion = {'text': '', 'url': ''}
Comment = {'id': '',
           'text': '',
           'user': User,
           'time': ''}
Trend = {'text': ''}

StatusText = {'text':'', 'blocks':[]}
RetweetStatus = {'id': '',
                 'text': StatusText,
                 'user': User,
                 'time': '',
                 'image': Image,
                 'favor_count': 0,
                 'retweet_count': 0,
                 'good_count': 0,
                 'comment_count': 0,
                 'comments': [],
                 'application': ''
                 }
Status = {'id': '',
          'text': StatusText,
          'user': User,
          'time': '',
          'image': Image,
          'favor_count': 0,
          'retweet_count': 0,
          'good_count': 0,
          'comment_count': 0,
          'comments':[],
          'application': '',
          'has_retweet': False
          }
Friends = {'follows' :[],
           'followers' : [],
           'friends' : []
           }

