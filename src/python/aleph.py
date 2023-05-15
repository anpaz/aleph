
import requests
import json

import logging
logger = logging.getLogger("aleph")

graph_baseurl = "http://localhost:5011/graph"
sample_baseurl = "http://localhost:5011/sample/classic"

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
        raise f"aleph server returned error: {r.reason}. {r}"

def _make_quantum(other):
    if isinstance(other, int):
        return Ket(_get(graph_baseurl + f"/{graph_id}/int?value={other}"))
    elif isinstance(other, bool):
        return Ket(_get(graph_baseurl + f"/{graph_id}/bool?value={other}"))
    else:
        return other

graph_id = _post(graph_baseurl)
logger.info("Working with graph: " + graph_id + " from url: " + graph_baseurl)

class Ket():
    def __init__(self, id):
        self.id = id

class KetBool():
    def __init__(self, id=None):
        if id:
            self.id = id
        else:
            self.id = _post(graph_baseurl + f"/{graph_id}/literal?width=1")

    def __and__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(graph_baseurl + f"/{graph_id}/and?left={self.id}&right={other.id}"))

    def __or__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(graph_baseurl + f"/{graph_id}/or?left={self.id}&right={other.id}"))

    def __str__(self) -> str:
        return f"|{self.id}⟩"


class KetInt:
    def __init__(self, size=3, id=None):
        if id:
            self.id = id
        else:
            self.id = _post(graph_baseurl + f"/{graph_id}/literal?width={size}")

    def __add__(self, other):
        other = _make_quantum(other)
        return KetInt(id=_get(graph_baseurl + f"/{graph_id}/add?left={self.id}&right={other.id}"))

    def __mul__(self, other):
        other = _make_quantum(other)
        return KetInt(id=_get(graph_baseurl + f"/{graph_id}/multiply?left={self.id}&right={other.id}"))

    def __eq__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(graph_baseurl + f"/{graph_id}/eq?left={self.id}&right={other.id}"))

    def __ne__(self, other):
        other = _make_quantum(other)
        ket=_get(graph_baseurl + f"/{graph_id}/eq?left={self.id}&right={other.id}")
        return KetBool(id=_get(graph_baseurl + f"/{graph_id}/not?ket={ket}"))

    def __le__(self, other):
        other = _make_quantum(other)
        return KetBool(id=_get(graph_baseurl + f"/{graph_id}/lte?left={self.id}&right={other.id}"))

    def __str__(self) -> str:
        return f"|{self.id}⟩"


def join(ids):
    logger.debug(f"Joining: {ids}")
    if isinstance(ids, list):
        if len(ids) == 1:
            return ids[0].id
        elif len(ids) == 2:
            return _get(graph_baseurl + f"/{graph_id}/join?left={ids[0].id}&right={ids[1].id}")
        else:
            head, *tail = ids
            tail = join(tail)
            return _get(graph_baseurl + f"/{graph_id}/join?left={head.id}&right={tail}")
    else:
        return ids.id

def filter(ids, expression: KetBool):
    ketId = join(ids)
    id = _get(graph_baseurl + f"/{graph_id}/filter?ket={ketId}&filter={expression.id}")
    if isinstance(ids, KetInt):
        return KetInt(id=id)
    elif isinstance(ids, KetBool):
        return KetBool(id=id)
    else:
        return Ket(id=id)

def sample(ids):
    ketId = join(ids)
    result = _post(sample_baseurl + f"/{graph_id}/{ketId}")
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

    node = _get(graph_baseurl + f"/{graph_id}/{ket.id}")
    node = json.loads(node)
    print ("digraph G {")
    print_one(node, "", 0)
    print ("}")
