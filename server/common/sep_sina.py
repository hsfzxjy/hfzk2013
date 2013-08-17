import re
import logging

reg_mention = (ur'@[\w-]+','mention',1)
reg_trend = (ur'#[^#]+#', 'trend',2)
reg_url = (ur'http\://t\.cn/[a-zA-Z0-9]{7}','url',0)
reg_emotion = (ur'\[.+\]', 'emotion', 0)

def make(exp, s):
    a = re.compile(exp[0],re.UNICODE)
    result = []
    for i in a.finditer(s):
        if exp[2]==0:
            extra = s[i.start():i.end()]
        elif exp[2] == 1:
            extra = s[i.start()+1:i.end()]
        else:
            extra = s[i.start()+1:i.end()-1]
        result.append((i.start(),i.end(),exp[1],extra))
    return result

def sep(text):
    text = unicode(text)
    tmp = []
    tmp += make(reg_mention,text)
    tmp += make(reg_trend,text)
    tmp += make(reg_url,text)
    tmp += make(reg_emotion, text)
    li = []
    pre = 0
    tmp.sort()
    for i in tmp:
        if text[pre:i[0]]:
            li.append({'text':text[pre:i[0]],'texttype':'normal'})
        li.append({'text':text[i[0]:i[1]],'texttype':i[2],'extra':i[3]})
        pre = i[1]
    if text[pre:]:
        li.append({'text':text[pre:],'texttype':'normal'})
    return {'text':text,'blocks':li}