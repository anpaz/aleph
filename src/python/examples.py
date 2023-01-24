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

def create_node():
    ket = aleph.KetInt(2)
    return aleph.filter(ket, ket <= 2)

nodes = [ create_node() for _ in range(4) ]
edges = [(0, 1), (1, 2), (0, 2),(1, 3) ]

# Compares all the edges, and tags as valid
# the values in the nodes for which its a valid 
# color combination:
def compare(nodes, edges):
    if len(edges) == 1:
        # get the nodes from the edge
        (l, r) = edges[0] 
        return nodes[l] != nodes[r]
    else:
        head, *tail = edges
        (l, r) = head 
        a = nodes[l] != nodes[r]
        b = compare(nodes, tail)
        return  a & b

answers = aleph.filter(nodes, compare(nodes, edges))
aleph.print_tree(answers)

print(aleph.sample(answers))