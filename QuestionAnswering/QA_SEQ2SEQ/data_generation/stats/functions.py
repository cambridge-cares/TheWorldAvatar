from collections import defaultdict
from typing import Iterable
from constants.functions import NumOp

from locate_then_ask.query_graph import QueryGraph


def count_functions(query_graphs: Iterable[QueryGraph]):
    stats = defaultdict(lambda:0)
    for query_graph in query_graphs:
        for s, o, p in query_graph.edges(data="label"):
            if p != "func":
                continue
            op = query_graph.nodes[o]["operator"]
            assert isinstance(op, NumOp) or isinstance(op, NumOp), op

            stats[op.value] += 1

    return stats