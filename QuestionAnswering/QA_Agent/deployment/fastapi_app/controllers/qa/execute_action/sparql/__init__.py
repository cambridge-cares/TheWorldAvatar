from functools import cache
import logging
import time
from typing import Annotated, Dict, List

from fastapi import Depends

from constants.prefixes import PREFIX_NAME2URI
from controllers.qa.model import QAStep
from services.entity_store import EntityStore, get_entity_store
from services.kg import KgClient
from utils.collections import FrozenDict
from controllers.qa.execute_action.model import SparqlAction, SparqlBinding
from .process_query import SparqlQueryProcessor, get_sparqlQuery_processor
from .kg import get_ns2kg
from .process_response import (
    SparqlResponseProcessor,
    get_sparqlRes_processor,
)


logger = logging.getLogger(__name__)


class SparqlActionExecutor:
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

    def exec(self, action: SparqlAction):
        steps: List[QAStep] = []

        logger.info("Performing entity linking for bindings: " + str(action.bindings))
        timestamp = time.time()
        var2iris = {
            binding.var: [
                iri
                for val in binding.values
                for iri in self.entity_store.link(
                    clsname=val.clsname, text=val.text, identifier=val.identifier
                )
            ]
            for binding in action.bindings
        }
        latency = time.time() - timestamp
        logger.info("IRIs: " + str(var2iris))

        logger.info("Input query:\n" + action.query)
        timestamp = time.time()
        query = self.query_processor.process(sparql=action.query, bindings=var2iris)
        latency = time.time() - timestamp
        logger.info("Processed query:\n" + query)
        steps.append(
            QAStep(
                action="postprocess_sparql",
                arguments={
                    "sparql": action.query,
                    "bindings": var2iris,
                },
                results=query,
                latency=latency,
            )
        )

        prefixed_query = self.PREFIXES + query
        logger.info(
            "Executing query at: " + self.ns2kg[action.namespace].sparql.endpoint
        )
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

        items = self.response_processor.process(
            vars=res["head"]["vars"], bindings=res["results"]["bindings"]
        )

        return steps, items


@cache
def get_sparqlAction_executor(
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    query_processor: Annotated[
        SparqlQueryProcessor, Depends(get_sparqlQuery_processor)
    ],
    response_processor: Annotated[
        SparqlResponseProcessor, Depends(get_sparqlRes_processor)
    ],
):
    return SparqlActionExecutor(
        ns2kg=ns2kg,
        entity_store=entity_store,
        query_processor=query_processor,
        response_processor=response_processor,
    )
