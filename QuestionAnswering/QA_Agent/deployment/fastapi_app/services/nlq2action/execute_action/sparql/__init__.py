from functools import cache
import logging
import time
from typing import Annotated, Dict, List

from fastapi import Depends

from services.entity_store import EntityStore, get_entity_linker
from services.kg import get_sgCompany_bgClient, get_sgPlot_bgClient, get_sg_ontopClient
from model.qa import QAData, QAStep
from core.kg import KgClient
from utils.collections import FrozenDict
from utils.rdf import flatten_sparql_response
from .postprocess import SparqlPostProcessor
from ..model import SparqlAction


logger = logging.getLogger(__name__)


class SparqlActionExecutor:
    PREFIXES = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

PREFIX unit: <http://qudt.org/vocab/unit/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>
PREFIX gml: <http://www.opengis.net/citygml/building/2.0/>
PREFIX grp: <http://www.opengis.net/citygml/cityobjectgroup/2.0/>

PREFIX bs: <https://www.theworldavatar.com/kg/ontobuildingstructure/>
PREFIX carpark:	<https://www.theworldavatar.com/kg/ontocarpark/>
PREFIX landplot: <https://www.theworldavatar.com/kg/landplot/>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>
PREFIX ontochemplant: <http://www.theworldavatar.com/kg/ontochemplant/>
PREFIX ontoplanreg: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX ontoplot: <https://www.theworldavatar.com/kg/ontoplot/>
PREFIX ontozoning: <https://www.theworldavatar.com/kg/ontozoning/>
PREFIX ub: <https://www.theworldavatar.com/kg/ontoubemmp/>

"""

    def __init__(self, ns2kg: Dict[str, KgClient], entity_linker: EntityStore):
        self.ns2kg = ns2kg
        self.postprocessor = SparqlPostProcessor(
            entity_linker, ns2uri={ns: kg.sparql.endpoint for ns, kg in ns2kg.items()}
        )
        self.entity_linker = entity_linker

    def exec(self, action: SparqlAction):
        steps: List[QAStep] = []

        logger.info("Input query:\n" + action.query)
        timestamp = time.time()
        query, varnames = self.postprocessor.postprocess(action.query)
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

        prefixed_query = self.PREFIXES + query
        logger.info("Executing query at: " + self.ns2kg[action.namespace].sparql.endpoint)
        timestamp = time.time()
        res = self.ns2kg[action.namespace].query(prefixed_query)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="execute_sparql",
                arguments=dict(namespace=action.namespace, query=query),
                latency=latency,
            )
        )

        vars, bindings = flatten_sparql_response(res)
        for varname in varnames:
            try:
                idx = vars.index(varname)
            except:
                continue
            vars.insert(idx + 1, varname + "Name")

        for binding in bindings:
            for varname in varnames:
                if varname in binding:
                    binding[varname + "Name"] = self.entity_linker.lookup_label(
                        binding[varname]
                    )

        return steps, QAData(vars=vars, bindings=bindings)


@cache
def get_ns2kg(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    plot_client: Annotated[KgClient, Depends(get_sgPlot_bgClient)],
    company_client: Annotated[KgClient, Depends(get_sgCompany_bgClient)],
):
    return FrozenDict(
        {"ontop": ontop_client, "plot": plot_client, "company": company_client}
    )


@cache
def get_sparqlAction_executor(
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
    entity_linker: Annotated[EntityStore, Depends(get_entity_linker)],
):
    return SparqlActionExecutor(ns2kg=ns2kg, entity_linker=entity_linker)
