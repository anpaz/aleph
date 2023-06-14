import logging
logging.basicConfig(level=logging.INFO)

from aleph_lang import KetInt, KetBool, sample, width_for, tree, histogram

# Randomly get a 0 or 1 value
def coin_flip():
    coin = KetBool()
    return sample(coin)

# Get a random number
def random_number():
    random = KetInt(width=10)
    return sample(random)

# Roll to dices and return the sum
def dice_roll():
    dice1 = KetInt().where_in(range(1,7))
    dice2 = KetInt().where_in(range(1,7))

    roll = dice1 + dice2
    return sample([dice1, dice2, roll])

# Roll to dices and return the histogram of the sum of their outcomes.
def dice_roll_histogram():
    dice1 = KetInt().where_in(range(1,7))
    dice2 = KetInt().where_in(range(1,7))

    roll = dice1 + dice2
    return histogram([roll], rounds=1000)

# Solve x + a == bx 
def solve_equation(a, b):
    x = KetInt()
    eq1 = x + a
    eq2 = b * x

    tree(eq1 == eq2)

    return sample(x, when=(eq1 == eq2))

# Solve a graph coloring problem, for the given number of nodes and list of edges.
def graph_coloring(max_colors, nodes_count, edges):
    def create_node():
        w = width_for(max_colors)
        return KetInt(width=w).where_less_than_equals(max_colors - 1)

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

    nodes = [ create_node() for _ in range(nodes_count) ]
    edges = [ (nodes[x], nodes[y]) for (x, y) in edges ]
    filter = compare(edges)

    ## Print to the console a graphviz representation
    ## of the quantum graph:
    ## tree(filter)

    return sample(nodes, when=filter)

print("coin flip:", coin_flip())
print("random number:", random_number())
print("dice roll:", dice_roll())
print("dice roll histogram: ", dice_roll_histogram())
print("solve x + 3 == 2 * x", solve_equation(3, 2))

max_colors = 3
total_nodes = 4
edges = [ (0, 1), (1, 2), (0, 2), (1, 3) ]
print("graph coloring:", graph_coloring(max_colors, total_nodes, edges))

