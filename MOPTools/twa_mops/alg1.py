import os
alg1_fpath = os.path.join(os.path.dirname(__file__), "algs_sparql", "alg1.sparql")
with open(".sparql", "r") as file:
    alg1 = file.read()
