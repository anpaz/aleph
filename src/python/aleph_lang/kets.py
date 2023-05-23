
import requests
import json
import math

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

def width_for(value):
    return int(math.ceil(math.log(value + 1, 2))) if value > 0 else 1

def _make_quantum(other):
    if isinstance(other, bool):
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/bool?value={other}"))
    elif isinstance(other, int):
        w = width_for(other)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/int?value={other}"), width=w)
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
    
    def where_true(self):
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/where?id={self.id}&op=id&arg={self.id}"))

    def And(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/and?left={self.id}&right={other.id}"))

    def __and__(self, other):
        return self.And(other)

    def __rand__(self, other):
        other = _make_quantum(other)
        return other.And(self)

    def Or(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/or?left={self.id}&right={other.id}"))

    def __or__(self, other):
        return self.Or(other)

    def __ror__(self, other):
        other = _make_quantum(other)
        return other.Or(self)

    def __bool__(self):
        raise Exception("Use the & and | operators for boolean expressions with Kets.")

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
        
    def where_in(self, values):
        values = ",".join(map(str, values))
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/where-in?id={self.id}&op=in&values={values}"))

    def add(self, other, width=None):
        other = _make_quantum(other)
        w = width if width else max(self.width, other.width)
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/map/add?left={self.id}&right={other.id}&width={w}"),width=w)

    def __add__(self, other):
        return self.add(other)

    def __radd__(self, other):
        other = _make_quantum(other)
        return other.add(self)

    def multiply(self, other, width=None):
        other = _make_quantum(other)
        w = width if width else self.width + other.width
        return KetInt(id=_get(aleph_baseurl + f"/{graph_id}/map/multiply?left={self.id}&right={other.id}&width={w}"),width=w)

    def __mul__(self, other):
        return self.multiply(other)

    def __rmul__(self, other):
        other = _make_quantum(other)
        return other.multiply(self)

    def equals(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/eq?left={self.id}&right={other.id}"))

    def __eq__(self, other):
        return self.equals(other)

    def __req__(self, other):
        other = _make_quantum(other)
        return other.equals(self)

    def not_equals(self, other):
        other = _make_quantum(other)
        ketId=_get(aleph_baseurl + f"/{graph_id}/map/eq?left={self.id}&right={other.id}")
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/not?id={ketId}"))

    def __ne__(self, other):
        return self.not_equals(other)

    def __rne__(self, other):
        other = _make_quantum(other)
        return other.not_equals(self)

    def less_than_equals(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/lte?left={self.id}&right={other.id}"))

    def __le__(self, other):
        return self.less_than_equals(other)

    def __ge__(self, other):
        other = _make_quantum(other)
        return other.less_than_equals(self)

    def greater_than(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(aleph_baseurl + f"/{graph_id}/map/gt?left={self.id}&right={other.id}"))

    def __gt__(self, other):
        return self.greater_than(other)

    def __lt__(self, other):
        other = _make_quantum(other)
        return other.greater_than(self)

    def __str__(self) -> str:
        return f"|{self.id}⟩"


def sample(kets, when=None):
    filter= "filter=-1" if when is None else  f"filter={when.id}"

    ketIds = ",".join(map(lambda ket: ket.id, kets)) if isinstance(kets, list) else kets.id
    result = _post(aleph_baseurl + f"/{graph_id}/~sample/?ids={ketIds}&{filter}")
    return json.loads(result)

def prepare(kets, when=None):
    filter= "filter=-1" if when is None else  f"filter={when.id}"

    ketIds = ",".join(map(lambda ket: ket.id, kets)) if isinstance(kets, list) else kets.id
    result = _get(aleph_baseurl + f"/{graph_id}/~prepare/?ids={ketIds}&{filter}")
    return json.loads(result)

def tree(ket):
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
