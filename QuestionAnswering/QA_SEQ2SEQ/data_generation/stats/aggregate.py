from collections import defaultdict
from typing import Iterable, Optional
from constants.functions import AggOp

from locate_then_ask.query_graph import QueryGraph


def count_aggregate(query_graphs: Iterable[QueryGraph]):
    stats = defaultdict(lambda: 0)
    for query_graph in query_graphs:
        for _, attr in query_graph.nodes(data=True):
            if not attr.get("question_node"):
                continue
            agg: Optional[AggOp] = attr.get("agg")
            if not agg:
                continue
            stats[agg.value] += 1

    return stats
