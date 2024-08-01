from collections import defaultdict
from typing import Iterable

from locate_then_ask.query_graph import QueryGraph


def count_solution_modifiers(query_graphs: Iterable[QueryGraph]):
    stats = defaultdict(lambda: 0)
    for query_graph in query_graphs:
        if query_graph.groupby:
            stats["group_by"] += 1
        if query_graph.orderby:
            stats["order_by"] += 1
        if query_graph.limit:
            stats["limit"] += 1

    return stats