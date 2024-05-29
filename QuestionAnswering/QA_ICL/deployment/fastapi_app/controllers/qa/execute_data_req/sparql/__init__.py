from functools import cache
import logging
import time
from typing import Annotated, Dict, List

from fastapi import Depends

from constants.prefixes import PREFIX_NAME2URI
from controllers.qa.model import QAStep
from services.entity_store import EntityStore, get_entity_store
from services.example_store.model import SparqlDataRequest
from services.kg import KgClient
from services.model import TableDataItem
from utils.collections import FrozenDict
from utils.rdf import flatten_sparql_response
from .process_query import SparqlQueryProcessor, get_sparqlQuery_processor
from .kg import get_ns2kg
from .process_response import (
    SparqlResponseProcessor,
    get_sparqlRes_processor,
)


logger = logging.getLogger(__name__)


class SparqlDataReqExecutor:
    PREFIXES = (
        "\n".join(
            "PREFIX {name}: <{uri}>".format(name=name, uri=uri)
            for name, uri in PREFIX_NAME2URI.items()
        )
        + "\n"
    )

    def __init__(
        self,
        ns2kg: Dict[str, KgClient],
        entity_store: EntityStore,
        query_processor: SparqlQueryProcessor,
        response_processor: SparqlResponseProcessor,
    ):
        self.ns2kg = ns2kg
        self.entity_store = entity_store
        self.query_processor = query_processor
        self.response_processor = response_processor

    def exec(self, req: SparqlDataRequest):
        steps: List[QAStep] = []

        logger.info("Performing entity linking for bindings: " + str(req.bindings))
        timestamp = time.time()
        var2iris = {
            binding.var: [
                iri
                for val in binding.values
                for iri in self.entity_store.link(
                    cls=binding.cls, text=val.text, identifier=val.identifier
                )
            ]
            for binding in req.bindings
        }
        latency = time.time() - timestamp
        logger.info("IRIs: " + str(var2iris))

        logger.info("Input query:\n" + req.query)
        timestamp = time.time()
        query = self.query_processor.process(sparql=req.query, bindings=var2iris)
        latency = time.time() - timestamp
        logger.info("Processed query:\n" + query)
        steps.append(
            QAStep(
                action="postprocess_sparql",
                arguments={
                    "sparql": req.query,
                    "bindings": var2iris,
                },
                results=query,
                latency=latency,
            )
        )

        prefixed_query = self.PREFIXES + query
        logger.info(
            "Executing query at: " + self.ns2kg[req.namespace].sparql.endpoint
        )
        timestamp = time.time()
        res = self.ns2kg[req.namespace].query(prefixed_query)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="execute_sparql",
                arguments=dict(namespace=req.namespace, query=query),
                latency=latency,
            )
        )

        vars, bindings = flatten_sparql_response(res)
        table = TableDataItem(vars=vars, bindings=bindings)

        self.response_processor.process(
            nodes_to_augment=req.bindings + req.nodes_to_augment, table=table
        )

        return steps, [table]


@cache
def get_sparqlReq_executor(
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    query_processor: Annotated[
        SparqlQueryProcessor, Depends(get_sparqlQuery_processor)
    ],
    response_processor: Annotated[
        SparqlResponseProcessor, Depends(get_sparqlRes_processor)
    ],
):
    return SparqlDataReqExecutor(
        ns2kg=ns2kg,
        entity_store=entity_store,
        query_processor=query_processor,
        response_processor=response_processor,
    )
