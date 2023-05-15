import logging
logging.basicConfig(level=logging.INFO)

import aleph


k1 = aleph.KetInt()
k2 = aleph.KetInt(size=5)
k3 = k1 + k2
k4 = k2 <= k1

s1 = aleph.sample([k1, k2, k3, k4])
s2 = aleph.sample([k2, k2])
s3 = aleph.sample(k2)

(t1, t2) = s2
(a1, a2, a3, a4) = s1

print(f"{k1}, {k2}, {k3}, {k4}, s1:{s1}, s2:{s2}, s3:{s3}, t1:{t1}")

if s3 > 3:
    print(f"{s3} > 3")
else:
    print(f"{s3} <= 3")

print(f"{a1} + {a2} = {a3}")
print(f"{a2} <= {a1} = {a4}")

f1 = aleph.filter(k3, k3 <= 2)
(a1, a2, a3) = aleph.sample([k1, k2, f1])

aleph.print_tree(f1)
print(f"a1: {a1}, a2:{a2}, a3:{a3}")


# -------------- #
# graph coloring #
# -------------- #
import math

MAX_COLORS = 3

# Each node is encoded as a Ket, we initialize a Ket
# with enough width for all colors, but filter
# its value accordingly
def create_node():
    w = math.ceil(math.log(MAX_COLORS, 2))
    ket = aleph.KetInt(w)
    return aleph.filter(ket, ket <= MAX_COLORS - 1)

# Recurisvely compares the nodes in all the edges:
def compare(edges):
    if len(edges) == 1:
        (left, right) = edges[0] 
        return left != right
    else:
        head, *tail = edges
        (left, right) = head
        a = left != right
        b = compare(tail)
        return  a & b

nodes = [ create_node() for _ in range(4) ]
edges = [ 
    (nodes[0], nodes[1]), 
    (nodes[1], nodes[2]), 
    (nodes[0], nodes[2]),
    (nodes[1], nodes[3])
]

answers = aleph.filter(nodes, compare(edges))
aleph.print_tree(answers)
print(aleph.sample(answers))
