from dataclasses import dataclass
import itertools
from typing import Iterable, Tuple

from .sparql_base import SparqlBase


class GraphPattern(SparqlBase):
    pass


@dataclass(order=True, frozen=True)
class ValuesClause(GraphPattern):
    var: str
    values: Tuple[str, ...]

    def __init__(self, var: str, values: Iterable[str]):
        object.__setattr__(self, "var", var)
        object.__setattr__(self, "values", tuple(values))

    def __str__(self):
        return "VALUES {var} {{ {values} }}".format(
            var=self.var,
            values=" ".join(
                [
                    '"{val}"'.format(val=val) if ":" not in val else val
                    for val in self.values
                ]
            ),
        )

    @classmethod
    def extract(cls, sparql_fragment: str):
        """VALUES ?var { {literal} {literal} ... }"""
        assert sparql_fragment.startswith("VALUES")
        sparql_fragment = sparql_fragment[len("VALUES") :].lstrip()
        assert sparql_fragment.startswith("?"), sparql_fragment
        var, sparql_fragment = sparql_fragment.split(maxsplit=1)
        
        sparql_fragment = sparql_fragment.lstrip()
        assert sparql_fragment.startswith("{"), sparql_fragment
        sparql_fragment = sparql_fragment[1:].lstrip()

        ptr = 0
        literals = []
        while ptr < len(sparql_fragment) and sparql_fragment[ptr] != "}":
            assert sparql_fragment[ptr] == '"', sparql_fragment
            _ptr = ptr + 1
            _ptr_literal_start = _ptr
            while _ptr < len(sparql_fragment) and sparql_fragment[_ptr] != '"':
                _ptr += 1
            assert sparql_fragment[_ptr] == '"'

            literal = sparql_fragment[_ptr_literal_start:_ptr]
            literals.append(literal)

            ptr = _ptr + 1
            while ptr < len(sparql_fragment) and sparql_fragment[ptr].isspace():
                ptr += 1

        return cls(var, literals), sparql_fragment[ptr + 1 :]

@dataclass(order=True, frozen=True)
class FilterClause(GraphPattern):
    constraint: str

    def __str__(self):
        return "FILTER ( {constraint} )".format(constraint=self.constraint)

    @classmethod
    def extract(cls, sparql_fragment: str):
        sparql_fragment = sparql_fragment[len("FILTER") :].strip()
        assert sparql_fragment.startswith("("), sparql_fragment

        sparql_fragment = sparql_fragment[1:].strip()
        ptr = 0
        quote_open = False
        while ptr < len(sparql_fragment) and (
            sparql_fragment[ptr] != ")" or quote_open
        ):
            if sparql_fragment[ptr] == '"':
                quote_open = not quote_open
            ptr += 1
        assert sparql_fragment[ptr] == ")", sparql_fragment
        constraint = sparql_fragment[:ptr].strip()

        return cls(constraint), sparql_fragment[ptr + 1 :]

@dataclass(order=True, frozen=True)
class TriplePattern(GraphPattern):
    subj: str
    # [(p1, o1), (p2, o2), ...]
    tails: Tuple[Tuple[str, str], ...]

    def __init__(self, subj: str, tails: Iterable[Tuple[str, str]]):
        object.__setattr__(self, "subj", subj)
        object.__setattr__(self, "tails", tuple(tails))

    @classmethod
    def from_triple(cls, subj: str, predicate: str, obj: str):
        return cls(subj=subj, tails=[(predicate, obj)])

    def __str__(self):
        return "{subj} {tails} .".format(
            subj=self.subj,
            tails=" ; ".join(
                [
                    "{predicate} {obj}".format(predicate=predicate, obj=obj)
                    for predicate, obj in self.tails
                ]
            ),
        )

    @classmethod
    def extract(cls, sparql_fragment: str):
        subj, sparql_fragment = sparql_fragment.split(maxsplit=1)

        tails = []
        while True:
            sparql_fragment = sparql_fragment.strip()
            predicate, sparql_fragment = sparql_fragment.split(maxsplit=1)

            sparql_fragment = sparql_fragment.strip()
            if sparql_fragment.startswith('"'):
                ptr = 1
                while ptr < len(sparql_fragment) and sparql_fragment[ptr] != '"':
                    ptr += 1
                obj = sparql_fragment[: ptr + 1]
                sparql_fragment = sparql_fragment[ptr + 1 :].strip()
            elif sparql_fragment.startswith('['):
                stack = ['[']
                ptr = 1
                obj = None
                while ptr < len(sparql_fragment):
                    if sparql_fragment[ptr] in "[":
                        stack.append('[')
                    elif sparql_fragment[ptr] == "]":
                        assert stack[-1] == '[', "Unexpected close square bracket: " + sparql_fragment
                        if len(stack) == 1:
                            obj = sparql_fragment[:ptr + 1]
                            sparql_fragment = sparql_fragment[ptr + 1 :].strip()
                            break
                        else:
                            stack.pop()
                    elif sparql_fragment[ptr] == '"':
                        ptr += 1
                        while ptr < len(sparql_fragment) and sparql_fragment[ptr] != '"':
                            ptr += 1
                    else:
                        pass
                    ptr += 1
                assert obj is not None
            else:
                splits = sparql_fragment.split(maxsplit=1)
                if len(splits) == 2:
                    obj, sparql_fragment = splits
                else:
                    (obj,) = splits
                    sparql_fragment = ""

                if obj.endswith(".") or obj.endswith(";"):
                    punctuation = obj[-1]
                    sparql_fragment = punctuation + sparql_fragment
                    obj = obj[:-1]

            tails.append((predicate, obj))

            assert sparql_fragment[0] in [";", "."], sparql_fragment
            punctuation = sparql_fragment[0]
            sparql_fragment = sparql_fragment[1:]
            if punctuation == ".":
                break

        return cls(subj=subj, tails=tails), sparql_fragment

@dataclass(order=True, frozen=True)
class OptionalClause(GraphPattern):
    graph_patterns: Tuple[GraphPattern, ...]

    def __init__(self, graph_patterns: Iterable[GraphPattern]):
        object.__setattr__(self, "graph_patterns", tuple(graph_patterns))

    def __str__(self):
        return "OPTIONAL {{\n{patterns}\n}}".format(
            patterns="\n".join(["  " + str(pattern) for pattern in self.graph_patterns])
        )

    def tolines(self):
        return list(
            itertools.chain.from_iterable(
                [
                    ["OPTIONAL {"],
                    [
                        "  " + str(line)
                        for pattern in self.graph_patterns
                        for line in pattern.tolines()
                    ],
                    ["}"],
                ]
            )
        )


@dataclass(order=True, frozen=True)
class BindClause(GraphPattern):
    exprn: str
    var: str

    def __str__(self):
        return "BIND ({expr} AS {var})".format(expr=self.exprn, var=self.var)

@dataclass(order=True, frozen=True)
class ServicePattern(GraphPattern):
    endpoint: str
    graph_patterns: Tuple[GraphPattern, ...]

    def __post_init__(self):
        if not isinstance(self.graph_patterns, tuple):
            object.__setattr__(self, "graph_patterns", tuple(self.graph_patterns))

    def __str__(self):
        return "SERVICE <{endpoint}> {{\n{group_graph_pattern}\n}}".format(
            endpoint=self.endpoint,
            group_graph_pattern="\n".join(
                [
                    "  " + line
                    for pattern in self.graph_patterns
                    for line in pattern.tolines()
                ]
            ),
        )