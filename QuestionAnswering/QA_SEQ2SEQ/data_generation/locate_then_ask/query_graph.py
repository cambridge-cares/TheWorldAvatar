from dataclasses import dataclass
from decimal import Decimal
from typing import Any, Iterable, List, Optional, Tuple, Union
import networkx as nx

from constants.functions import AggOp, NumOp, StrOp


@dataclass
class OrderCond:
    var: str
    desc: int = False

class QueryGraph(nx.DiGraph):
    _LITERAL_PREFIX = "Literal_"
    _BN_PREFIX = "BN_"
    _FUNC_PREFIX = "Func_"
    _TOPICNODE_ATTRNAME = "topic_node"
    _GROUPBY_KEY = "GroupBy"
    _ORDERBY_KEY = "OrderBy"
    _LIMIT_KEY = "Limit"

    @classmethod
    def is_literal_node(cls, n: str):
        return n.startswith(cls._LITERAL_PREFIX)

    @classmethod
    def is_blank_node(cls, n: str):
        return n.startswith(cls._BN_PREFIX)

    def get_preds(self, subj: str):
        return [p for u, _, p in self.edges(data="label") if u == subj]

    def get_objs(self, subj: str, predicate: str):
        return [
            v
            for u, v, label in self.edges(data="label")
            if u == subj and label == predicate
        ]

    def add_topic_node(self, n: str, iri: Optional[str] = None):
        self.add_node(n, topic_entity=True, iri=iri)

    def make_literal_node(self, value: Union[str, Decimal], key: Optional[Any] = None):
        """Adds a grounded literal node to the graph and returns the node."""
        literal_num = sum(n.startswith(self._LITERAL_PREFIX) for n in self.nodes())
        n = self._LITERAL_PREFIX + str(literal_num)

        self.add_node(n, literal=True, template_node=True, label=value, key=key)

        return n

    def add_literal_node(self, n: str, key: Optional[Any] = None):
        self.add_node(n, literal=True, key=key)

    def make_blank_node(self, key: Optional[Any] = None):
        """Adds a blank node to the graph and returns the node."""
        bn_num = sum(n.startswith(self._BN_PREFIX) for n in self.nodes())
        n = self._BN_PREFIX + str(bn_num)

        self.add_node(n, blank_node=True, key=key)

        return n

    def add_iri_node(self, iri: str, prefixed: bool, key: Optional[Any] = None):
        self.add_node(iri, iri=iri, template_node=True, prefixed=prefixed, key=key)

    def add_func(self, target_node: str, operator: Union[NumOp, StrOp], operand: Any):
        func_num = sum(n.startswith(self._FUNC_PREFIX) for n in self.nodes())
        func_node = self._FUNC_PREFIX + str(func_num)
        func_label = operator.value + "\n" + str(operand)

        self.add_node(
            func_node,
            func=True,
            template_node=True,
            operator=operator,
            operand=operand,
            label=func_label,
        )
        self.add_edge(target_node, func_node, label="func")

    def add_question_node(self, n: str, agg: Optional[AggOp] = None):
        self.add_node(n, question_node=True, agg=agg)

    def add_triple(self, s: str, p: str, o: str):
        self.add_edge(s, o, label=p)

    def add_triples(self, triples: Iterable[Tuple[str, str, str]]):
        self.add_edges_from([(s, o, dict(label=p)) for s, p, o in triples])

    def add_groupby(self, n: str):
        if self._GROUPBY_KEY not in self.graph:
            self.graph[self._GROUPBY_KEY] = [n]
        else:
            self.graph[self._GROUPBY_KEY].append(n)

    def add_orderby(self, n: str, desc: bool = False):
        cond = OrderCond(desc=desc, var=n)
        if self._ORDERBY_KEY not in self.graph:
            self.graph[self._ORDERBY_KEY] = [cond]
        else:
            self.graph[self._ORDERBY_KEY].append(cond)

    def set_limit(self, limit: int):
        self.graph[self._LIMIT_KEY] = limit

    @property
    def groupby(self):
        return tuple(self.graph.get(self._GROUPBY_KEY, []))
    
    @property
    def orderby(self):
        return tuple(self.graph.get(self._ORDERBY_KEY, []))
    
    @property
    def limit(self):
        return self.graph.get(self._LIMIT_KEY, None)