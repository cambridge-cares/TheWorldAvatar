from functools import cache
from typing import Annotated

from fastapi import Depends

from services.kg import KgClient, get_ontokin_bgClient
from services.stores.entity_store.base import IEntityLinker
from utils.rdf import flatten_sparql_select_response


class ReactionLinker(IEntityLinker):
    def __init__(self, bg_client: KgClient):
        self.bg_client = bg_client

    def link(self, text: str | None, **kwargs) -> list[str]:
        if "equation" not in kwargs:
            return []

        query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
        
SELECT * WHERE {{
    ?Reaction okin:hasEquation "{equation}"
}}""".format(
            equation=kwargs["equation"]
        )

        res = self.bg_client.querySelect(query)
        _, bindings = flatten_sparql_select_response(res)
        return [binding["Reaction"] for binding in bindings]


@cache
def get_reaction_linker(bg_client: Annotated[KgClient, Depends(get_ontokin_bgClient)]):
    return ReactionLinker(bg_client=bg_client)
