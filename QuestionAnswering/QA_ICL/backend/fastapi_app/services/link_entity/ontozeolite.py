import logging
from typing import Annotated

from fastapi import Depends
from constants.namespace import ONTOZEOLITE
from model.entity_linking.ontozeolite import (
    ZeoliteFrameworkLinkingArgs,
    ZeoliticMaterialLinkingArgs,
)
from services.sparql import SparqlClient, get_ontozeolite_endpoint
from .base import LinkerManager


logger = logging.getLogger(__name__)


class OntozeoliteLinkerManager(LinkerManager):
    def __init__(self, ontozeolite_endpoint: str):
        self.sparql_client = SparqlClient(ontozeolite_endpoint)

    @property
    def cls2linker(self):
        return {
            "zeo:ZeoliteFramework": self.linkFramework,
            "zeo:ZeoliticMaterial": self.linkMaterial,
        }

    def linkFramework(self, text: str | None, **kwargs):
        try:
            args = ZeoliteFrameworkLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking zeolite framework with args: {args.model_dump_json()}")

        query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT ?Framework
WHERE {{
    ?Framework zeo:hasFrameworkCode "{args.code}" .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [row["Framework"] for row in bindings]
        return iris

    def linkMaterial(self, text: str | None, **kwargs):
        try:
            args = ZeoliticMaterialLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking zeolitic material with args: {args.model_dump_json()}")

        query = f"""PREFIX zeo: <{ONTOZEOLITE}>

SELECT ?Material
WHERE {{
    ?Material zeo:hasChemicalFormula "{args.formula}" .
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [row["Material"] for row in bindings]
        return iris


def get_ontozeolite_linkerManager(
    ontozeolite_endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return OntozeoliteLinkerManager(ontozeolite_endpoint=ontozeolite_endpoint)
