from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from constants.namespace import ONTOKIN, ONTOPROVENANCE
from model.entity_linking.ontokin import MechanismLinkingArgs, ReactionLikingArgs
from services.sparql import SparqlClient, get_ontokin_endpoint
from .base import LinkerManager


logger = logging.getLogger(__name__)


class OntokinLinkerManager(LinkerManager):
    def __init__(self, ontokin_endpoint: str):
        self.sparql_client = SparqlClient(ontokin_endpoint)

    @property
    def cls2linker(self):
        return {
            "okin:ReactionMechanism": self.linkMechanism,
            "ocape:ChemicalReaction": self.linkReaction,
        }

    def linkMechanism(self, text: str | None, **kwargs):
        try:
            args = MechanismLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking mechanism with args: {args.model_dump_json()}")

        query = f"""PREFIX okin: <{ONTOKIN}>
PREFIX op: <{ONTOPROVENANCE}>

SELECT DISTINCT *
WHERE {{
    ?Mechanism okin:hasProvenance/(op:hasDOI|op:hasURL) "{args.provenance}"
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["Mechanism"] for binding in bindings]
        return iris

    def linkReaction(self, text: str | None, **kwargs) -> list[str]:
        try:
            args = ReactionLikingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking reaction with args: {args.model_dump_json()}")

        query = f"""PREFIX okin: <{ONTOKIN}>
        
SELECT DISTINCT *
WHERE {{
    ?Reaction okin:hasEquation "{args.equation}"
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["Reaction"] for binding in bindings]
        return iris


@cache
def get_ontokin_linkerManager(
    ontokin_endpoint: Annotated[str, Depends(get_ontokin_endpoint)]
):
    return OntokinLinkerManager(ontokin_endpoint=ontokin_endpoint)
