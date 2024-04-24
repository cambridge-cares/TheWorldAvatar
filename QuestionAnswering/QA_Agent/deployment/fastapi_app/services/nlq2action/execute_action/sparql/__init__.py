from functools import cache
import logging
import time
from typing import Annotated, Dict, List

from fastapi import Depends

from services.kg import get_sgPlot_bgClient, get_sg_ontopClient
from model.qa import QAData, QAStep
from services.core.kg import KgClient
from services.utils.collections import FrozenDict
from services.utils.rdf import flatten_sparql_response
from .postprocess import SparqlPostProcessor, get_sparql_postprocessor
from ..model import SparqlAction


logger = logging.getLogger(__name__)


class SparqlActionExecutor:
    PREFIXES = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

PREFIX unit: <http://qudt.org/vocab/unit/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
PREFIX gml: <http://www.opengis.net/citygml/building/2.0/>

PREFIX bs: <https://www.theworldavatar.com/kg/ontobuildingstructure/>
PREFIX carpark:	<https://www.theworldavatar.com/kg/ontocarpark/>
PREFIX landplot: <https://www.theworldavatar.com/kg/landplot/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontoplanreg: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontoplot: <https://www.theworldavatar.com/kg/ontoplot/>
PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>
PREFIX ub: <https://www.theworldavatar.com/kg/ontoubemmp/>

"""

    def __init__(self, ns2kg: Dict[str, KgClient], postprocessor: SparqlPostProcessor):
        self.ns2kg = ns2kg
        self.postprocessor = postprocessor

    def exec(self, action: SparqlAction):
        steps: List[QAStep] = []

        logger.info("Input query:\n" + action.query)
        timestamp = time.time()
        query = self.postprocessor.postprocess(action.query)
        latency = time.time() - timestamp
        logger.info("Processed query:\n" + query)
        steps.append(
            QAStep(
                action="postprocess_sparql",
                arguments=action.query,
                results=query,
                latency=latency,
            )
        )

        query = self.PREFIXES + query
        timestamp = time.time()
        res = self.ns2kg[action.namespace].query(query)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="execute_sparql",
                arguments=dict(namespace=action.namespace, query=query),
                latency=latency,
            )
        )

        vars, bindings = flatten_sparql_response(res)
        return steps, QAData(vars=vars, bindings=bindings)


@cache
def get_ns2kg(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    plot_client: Annotated[KgClient, Depends(get_sgPlot_bgClient)],
):
    return FrozenDict({"ontop": ontop_client, "plot": plot_client})


@cache
def get_sparqlAction_executor(
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
    postprocessor: Annotated[SparqlPostProcessor, Depends(get_sparql_postprocessor)],
):
    return SparqlActionExecutor(ns2kg=ns2kg, postprocessor=postprocessor)
