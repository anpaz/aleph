
import requests
import json

import logging
logger = logging.getLogger("aleph")

aleph_baseurl = "http://localhost:5011/graph"

def _get(url):
    logger.debug("Sending: " + url)
    r = requests.get(url)
    logger.debug(f"Received: {r}")
    if r.ok:
        return r.text
    else:
        raise Exception(f"aleph server returned error: {r.reason}. {r}")

def _post(url):
    logger.debug("Sending: " + url)
    r = requests.post(url)
    logger.debug(f"Received: {r}")
    if r.ok:
        return r.text
    else:
        raise Exception(f"aleph server returned error: {r.reason}. {r}")

def _make_quantum(other):
    if isinstance(other, int):
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/int?value={other}"))
    elif isinstance(other, bool):
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/bool?value={other}"))
    else:
        return other

graph_id = _post(aleph_baseurl)
logger.info("Working with graph: " + graph_id + " from url: " + aleph_baseurl)

class Ket():
    def __init__(self, id):
        self.id = id

class KetBool():
    def __init__(self, id=None):
        if id:
            self.id = id
        else:
            self.id = _post(aleph_baseurl + f"/{graph_id}/literal?width=1")
        self.width = 1
        
    def __and__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/and?left={self.id}&right={other.id}"))

    def __or__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/or?left={self.id}&right={other.id}"))

    def __str__(self) -> str:
        return f"|{self.id}⟩"


class KetInt:
    def __init__(self, width=3, id=None):
        if id:
            self.id = id
        else:
            self.id = _post(aleph_baseurl + f"/{graph_id}/literal?width={width}")
        self.width = width

    def where_equals(self, value):
        other = _make_quantum(value)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/where?id={self.id}&op=eq&arg={other.id}"))

    def where_less_than_equals(self, value):
        other = _make_quantum(value)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/where?id={self.id}&op=lte&arg={other.id}"))

    def where_greater_than(self, value):
        other = _make_quantum(value)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/where?id={self.id}&op=gt&arg={other.id}"))
        
    def add(self, other):
        other = _make_quantum(other)
        w = max(self.width, other.width)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/map/add?left={self.id}&right={other.id}&width={w}"))

    def __add__(self, other):
        return self.add(other)

    def __radd__(self, other):
        other = _make_quantum(other)
        return other.add(self)

    def __mul__(self, other):
        other = _make_quantum(other)
        w = max(self.width, other.width)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/map/multiply?left={self.id}&right={other.id}&width={w}"))

    def __eq__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/eq?left={self.id}&right={other.id}"))

    def __ne__(self, other):
        other = _make_quantum(other)
        ketId=_get(aleph_baseurl + f"/{graph_id}/map/eq?left={self.id}&right={other.id}")
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/not?ket={ketId}"))

    def __le__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/lte?left={self.id}&right={other.id}"))

    def __gt__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/gt?left={self.id}&right={other.id}"))

    def __str__(self) -> str:
        return f"|{self.id}⟩"


def sample(kets, when=None):
    filter= f"filter={when.id}" if when else "filter=-1"

    ketIds = ",".join(map(lambda ket: ket.id, kets))
    result = _post(aleph_baseurl + f"/{graph_id}/~sample/?ids={ketIds}&{filter}")
    return json.loads(result)

def prepare(kets, when=None):
    filter= f"filter={when.id}" if when else "filter=-1"

    ketIds = ",".join(map(lambda ket: ket.id, kets))
    result = _get(aleph_baseurl + f"/{graph_id}/~prepare/?ids={ketIds}&{filter}")
    return json.loads(result)

def print_tree(ket):
    def label (node):
        return  '"' + str(node['id']) + ': ' + node['label'] + '"'

    def print_one(node, src, indent):
        space = " " * indent
        src = label(node)

        for d in node['dependencies']:
            if indent > 0:
                print("  " + src + " -> " + label(d))
            print_one(d, src, indent + 2)

    node = _get(aleph_baseurl + f"/{graph_id}/{ket.id}")
    node = json.loads(node)
    print ("digraph G {")
    print_one(node, "", 0)
    print ("}")
