from functools import cache
import logging
from typing import Annotated, Dict

from fastapi import Depends

from services.connectors.sg_land_lots.kg import get_sgLandLots_bgClient
from services.connectors.sg import get_sg_ontopClient
from model.qa import QAData
from services.core.kg import KgClient
from services.utils.rdf import flatten_sparql_response
from ..link_entity import ELMediator, get_elMediator
from .model import SparqlAction


logger = logging.getLogger(__name__)


class SparqlEntityLinker:
    def __init__(self, el_mediator: ELMediator):
        self.el_mediator = el_mediator

    def link(self, token: str):
        # '<LandUseType:\"residential\">' -> ['<https://example.org/LandUseType_1>', '<https://example.org/LandUseType_2>']
        if not token.startswith("<") or not token.endswith(">"):
            return [token]

        try:
            entity_type, surface_form = token[1:-1].split(":", maxsplit=1)
        except:
            return [token]

        if not surface_form.startswith('"') or not surface_form.endswith('"'):
            return [token]
        surface_form = surface_form[1:-1]

        try:
            iris = self.el_mediator.link(entity_type, surface_form)
        except Exception as e:
            logger.error("Error during entity linking: " + str(e))
            return [token]

        return ["<{iri}>".format(iri=iri) for iri in iris]


class SparqlPostProcessor:
    def __init__(self, entity_linker: SparqlEntityLinker):
        self.entity_linker = entity_linker

    def postprocess(self, sparql: str):
        idx = 0
        while idx < len(sparql):
            print(idx)
            # VALUES ?LandUseType { <LandUseType:\"residential\"> }
            idx_start = sparql.find("VALUES", idx)
            if idx_start >= len(sparql):
                break

            idx_start += len("VALUES") + 1
            while idx_start < len(sparql) and sparql[idx_start].isspace():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            # ?LandUseType { <LandUseType:\"residential\"> }
            if sparql[idx_start] != "?":
                break

            idx_start += 1
            while idx_start < len(sparql) and sparql[idx_start].isalnum():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            while idx_start < len(sparql) and sparql[idx_start].isspace():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            # { <LandUseType:\"residential\"> }
            if sparql[idx_start] != "{":
                break

            # assume no '}' within VALUES
            idx_end = sparql.find("}", idx_start + 1)
            if idx_end >= len(sparql):
                break

            values = " ".join(
                value
                for token in sparql[idx_start + 1 : idx_end].strip().split()
                for value in self.entity_linker.link(token)
            )

            sparql = "{before}{{ {values} }}{after}".format(
                before=sparql[:idx_start], values=values, after=sparql[idx_end + 1 :]
            )
            idx = idx_start + 2 + len(values) + 2
        return sparql


class SparqlActionExecutor:
    def __init__(self, ns2kg: Dict[str, KgClient], postprocessor: SparqlPostProcessor):
        self.ns2kg = ns2kg
        self.postprocessor = postprocessor

    def exec(self, action: SparqlAction):
        query = self.postprocessor.postprocess(action.query)
        res = self.ns2kg[action.namespace].query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)


@cache
def get_sparql_entityLinker(
    el_mediator: Annotated[ELMediator, Depends(get_elMediator)]
):
    return SparqlEntityLinker(el_mediator)


@cache
def get_sparql_postprocessor(
    entity_linker: Annotated[SparqlEntityLinker, Depends(get_sparql_entityLinker)]
):
    return SparqlPostProcessor(entity_linker)


@cache
def get_ns2kg(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    plot_client: Annotated[KgClient, Depends(get_sgLandLots_bgClient)],
):
    return {"ontop": ontop_client, "plot": plot_client}


@cache
def get_sparqlAction_executor(
    ns2kg: Annotated[Dict[str, KgClient], Depends(get_ns2kg)],
    postprocessor: Annotated[SparqlPostProcessor, Depends(get_sparql_postprocessor)],
):
    return SparqlActionExecutor(ns2kg=ns2kg, postprocessor=postprocessor)
