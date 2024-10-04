from dataclasses import dataclass
from typing import Optional

from .query_form import SelectClause
from .solution_modifier import SolutionModifier
from .sparql_base import SparqlBase
from .where_clause import WhereClause


@dataclass(order=True, frozen=True)
class SparqlQuery(SparqlBase):
    select_clause: SelectClause
    where_clause: WhereClause
    solution_modifier: Optional[SolutionModifier] = None

    def __str__(self):
        text = "{select_clause} {where_clause}".format(
            select_clause=self.select_clause,
            where_clause=self.where_clause,
        )
        if self.solution_modifier:
            text += "\n" + str(self.solution_modifier)
        return text

    @classmethod
    def fromstring(cls, sparql: str):
        select_clause, sparql_fragment = SelectClause.extract(sparql)
        where_clause, sparql_fragment = WhereClause.extract(sparql_fragment)
        solution_modifier, sparql_fragment = SolutionModifier.extract(sparql_fragment)
        assert not sparql_fragment or sparql_fragment.isspace(), sparql_fragment
        return cls(select_clause, where_clause, solution_modifier)
