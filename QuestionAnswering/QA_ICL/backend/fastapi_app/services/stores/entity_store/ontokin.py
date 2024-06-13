from functools import cache
from typing import Annotated

from fastapi import Depends

from services.sparql import SparqlClient, get_ontokin_endpoint
from services.stores.entity_store.base import IEntityLinker


class MechanismLinker(IEntityLinker):
    def __init__(self, ontokin_endpoint: str):
        self.sparql_client = SparqlClient(ontokin_endpoint)

    def link(self, text: str | None, **kwargs):
        if "provenance" not in kwargs:
            return []

        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>

SELECT DISTINCT *
WHERE {{
    ?Mechanism okin:hasProvenance/(op:hasDOI|op:hasURL) "{provenance}"
}}""".format(
            provenance=kwargs["provenance"]
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["Mechanism"] for binding in bindings]


@cache
def get_mechanism_linker(endpoint: Annotated[str, Depends(get_ontokin_endpoint)]):
    return MechanismLinker(ontokin_endpoint=endpoint)


class ReactionLinker(IEntityLinker):
    def __init__(self, ontokin_endpoint: SparqlClient):
        self.sparql_client = SparqlClient(ontokin_endpoint)

    def link(self, text: str | None, **kwargs) -> list[str]:
        if "equation" not in kwargs:
            return []

        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
        
SELECT DISTINCT *
WHERE {{
    ?Reaction okin:hasEquation "{equation}"
}}""".format(
            equation=kwargs["equation"]
        )

        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        return [binding["Reaction"] for binding in bindings]


@cache
def get_reaction_linker(endpoint: Annotated[str, Depends(get_ontokin_endpoint)]):
    return ReactionLinker(ontokin_endpoint=endpoint)
