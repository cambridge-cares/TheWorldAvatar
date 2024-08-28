from dataclasses import dataclass
from typing import Tuple

from .abc import Constraint
from .graph_pattern import GraphPattern


@dataclass(frozen=True, order=True)
class BrackettedExpression(Constraint):
    expression: str

    def __str__(self):
        return "( {exprn} )".format(exprn=self.expression)


@dataclass(frozen=True, order=True)
class NotExistsFunc(Constraint):
    graph_patterns: Tuple[GraphPattern, ...]

    def __str__(self):
        return "NOT EXISTS {{\n{group_graph_pattern}\n}}".format(
            group_graph_pattern="\n".join(
                [
                    "  " + line
                    for pattern in self.graph_patterns
                    for line in pattern.tolines()
                ]
            ),
        )
