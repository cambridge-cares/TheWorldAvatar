from dataclasses import dataclass
from typing import List

from core.sparql.sparql_base import SparqlBase


@dataclass
class SelectClause(SparqlBase):
    vars: List[str]


