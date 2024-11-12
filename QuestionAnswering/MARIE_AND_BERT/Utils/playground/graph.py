import matplotlib.pyplot as plt
import networkx as nx

import itertools as it
from networkx.utils import pairwise


G = nx.Graph()
G.add_node(1)
G.add_node(2)
G.add_node(3)
G.add_node(4)
G.add_node(5)
G.add_node(6)
G.add_node(6)
G.add_node(6)
G.add_node(6)
G.add_node(7)
G.add_node(8)
G.add_node(9)
G.add_edge(1, 2)
G.add_edge(1, 3)
G.add_edge(1, 4)
G.add_edge(1, 5)
G.add_edge(2, 6)
G.add_edge(3, 4)
G.add_edge(2, 5)
G.add_edge(4, 6)
G.add_edge(7, 6)
G.add_edge(8, 6)
G.add_edge(9, 6)
nx.draw(G, with_labels=True)
plt.show()

d = sorted(map(sorted, nx.k_edge_subgraphs(G, k=6)))
# d = list(nx.connected_components(G))
# for _d in d:
#     print(_d)
print(d)