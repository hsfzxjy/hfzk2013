from xml.dom import minidom
import json
from StringIO import StringIO as sio

impl = minidom.getDOMImplementation()
dom = impl.createDocument(None, "_BASE", None)
toxml = lambda(text):minidom.parseString(text).documentElement

def xml_to_object(text):

    def get_type(ss):
        s = ss.getAttribute("type")
        res = None
        if s == 'str':
            res = unicode
        elif s == 'int':
            res = int
        elif s == 'dict':
            res = dict
        elif s == 'bool':
            res = bool
        elif s == 'list':
            res = list
        return res

    def do_iter(text, type):
        _xml = toxml(text)
        result = [] if type == list else {}
        for node in _xml.childNodes:
            _type = get_type(node)
            tmp = None
            if _type in (dict,list):
                tmp = do_iter(node.toxml(), _type)
            else:
                tmp = do_else(node.toxml())
            if type == list:
                result.append(tmp)
            else:
                result[node.nodeName] = tmp
        return result

    def do_else(text):
        _xml = toxml(text)
        _type = get_type(_xml)
        return _type(_xml.childNodes[0].nodeValue)
    
    return do_iter(toxml(text).toxml(), dict)

def object_to_xml(obj):

    def do_list(node, li):
        node.setAttribute('type', 'list')
        for i in li:
            item = dom.createElement("_ITEM")
            if isinstance(i, list) or isinstance(i, tuple):
                do_list(item, i)
            elif isinstance(i, dict):
                do_dict(item, i)
            else:
                do_else(item, i)
            node.appendChild(item)
            
    def do_dict(node, di):
        node.setAttribute('type','dict')
        for i,j in di.iteritems():
            item = dom.createElement(str(i))
            if isinstance(j, list) or isinstance(j, tuple):
                do_list(item, j)
            elif isinstance(j, dict):
                do_dict(item, j)
            else:
                do_else(item, j)
            node.appendChild(item)

    def do_else(node, obj):
        node.appendChild(dom.createTextNode(unicode(obj)))
        attr = ''
        if isinstance(obj, unicode) or isinstance(obj, str):
            attr = 'str'
        elif isinstance(obj, bool):
            attr = 'bool'
        elif isinstance(obj, int):
            attr = 'int'
        node.setAttribute('type',attr)
                        
    result= dom.documentElement
    do_dict(result, obj)
    return result

json_to_object = lambda (j):json.load(sio(j))
object_to_json = lambda (obj):json.dumps(obj)
json_to_xml = lambda (j):object_to_xml(json_to_object(j))
xml_to_json = lambda (xml):object_to_json(xml_to_object(xml))
