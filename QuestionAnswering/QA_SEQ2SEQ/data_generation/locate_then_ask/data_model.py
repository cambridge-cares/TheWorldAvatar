
from dataclasses import dataclass

from locate_then_ask.query_graph import QueryGraph


@dataclass
class AskDatum:
    query_graph: QueryGraph
    query_sparql: str
    verbalization: str

