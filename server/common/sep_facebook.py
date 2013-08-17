import re
import logging

exp_trend = ur'#\w+'
exp_url = ur'(http|ftp|https):\/\/[\w\-_]+(\.[\w\-_]+)+([\w\-\.,@?^=%&:/~\+#]*[\w\-\@?^=%&/~\+#])?'

def make(s,exp_str,tag):
    a = re.compile(exp_str,re.UNICODE)
    result = []
    for i in a.finditer(s):
        result.append((i.start(),i.end(),tag,s[i.start():i.end()]))
    return result

def sep(status):
    text = ''
    if 'message' in status:
        text = status['message'] 
    elif 'story' in status:
        text = status['story']
    else:
        return {'text':'','blocks':[]}
    text = unicode(text)
    tmp = make(text, exp_trend, 'trend')
    tmp += make(text, exp_url, 'url')
    if 'message_tags' in status:
        if isinstance(status['message_tags'], dict):
            for i in status['message_tags'].itervalues():
                i = i[0]
                tmp+=[(i['offset'],i['offset']+i['length'],'mention',str(i['id']))]
        elif isinstance(status['message_tags'], list):
            for i in status['message_tags']:
                tmp+=[(i['offset'],i['offset']+i['length'],'mention',str(i['id']))]            
    li = []
    pre = 0
    tmp.sort()
    for i in tmp:
        logging.info(i)
        if text[pre:i[0]]:
            li.append({'text':text[pre:i[0]],'texttype':'normal'})
        li.append({'text':text[i[0]:i[1]],'texttype':i[2],'extra':i[3]})
        pre = i[1]
    if text[pre:]:
        li.append({'text':text[pre:],'texttype':'normal'})
    return {'text':text,'blocks':li}