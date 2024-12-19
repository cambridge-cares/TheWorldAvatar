# NOTE if we would like to allow CBUs functioning as 3-planar to function as 2-bent in new MOPs
# we can change "=" in `filter (?gbu_modularity >= ?metal_gbu_modularity)` and `filter (?_gbu_modularity >= ?organic_gbu_modularity)`
# to ">="
import os
alg2_fpath = os.path.join(os.path.dirname(__file__), "algs_sparql", "alg2.sparql")
with open(alg2_fpath, "r") as file:
    alg2 = file.read()
