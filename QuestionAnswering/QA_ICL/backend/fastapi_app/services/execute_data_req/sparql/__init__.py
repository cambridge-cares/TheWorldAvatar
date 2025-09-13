from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from constants.namespace import PREFIX2URI
from constants.prefixes import OM_MEASURE_ABOX_PREFIX, TWA_ABOX_PREFIXES
from model.exceptions.execute_data_req.sparql import TriplestoreNotFound
from model.nlq2datareq import SparqlDataReqForm
from model.structured_answer import DataItem, DocumentCollection, TableData
from services.sparql import SparqlClient
from utils.json import deep_pd_json_normalize_list
from utils.rdf import filter_deep_remove_iris_from_list
from .process_query import SparqlQueryProcessor, get_sparqlQuery_processor
from .endpoints import get_ns2endpoint
from .transform_response import (
    SparqlResponseTransformer,
    get_sparqlRes_transformer,
)


logger = logging.getLogger(__name__)


class SparqlDataReqExecutor:
    PREFIXES = (
        "\n".join(
            "PREFIX {name}: <{uri}>".format(name=name, uri=uri)
            for name, uri in PREFIX2URI.items()
        )
        + "\n"
    )

    def __init__(
        self,
        ns2endpoint: dict[str, str],
        query_processor: SparqlQueryProcessor,
        response_processor: SparqlResponseTransformer,
    ):
        self.ns2kg = {
            ns: SparqlClient(endpoint) for ns, endpoint in ns2endpoint.items()
        }
        self.query_processor = query_processor
        self.response_processor = response_processor

    def exec(
        self,
        var2cls: dict[str, str],
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, str],
        req_form: SparqlDataReqForm,
        vis_vars: list[str],
    ):
        logger.info("Unprocessed query:\n" + req_form.query)
        query = self.query_processor.process(
            sparql=req_form.query,
            entity_bindings=entity_bindings,
            const_bindings=const_bindings,
        )
        logger.info("Processed query:\n" + query)

        kg = self.ns2kg.get(req_form.triplestore)
        if kg is None:
            raise TriplestoreNotFound(
                f"Triplestore {req_form.triplestore} is not found; registered triplestores are: {', '.join(self.ns2kg.keys())}"
            )

        prefixed_query = self.PREFIXES + query
        logger.info("Executing query at: " + kg.sparql.endpoint)
        vars, bindings = kg.querySelectThenFlatten(prefixed_query)
        vis_var2iris = {
            var: list(
                set(
                    entity_bindings.get(var, [])
                    + [binding[var] for binding in bindings if var in binding]
                )
            )
            for var in vis_vars
        }

        logger.info("Transforming SPARQL response to documents...")
        docs = self.response_processor.transform(
            sparql_client=kg,
            var2cls=var2cls,
            vars=vars,
            bindings=bindings,
            pkeys=req_form.pkeys,
        )
        docs_collection = DocumentCollection(data=docs)
        logger.info("Done")

        logger.info("Linearising documents into a table...")
        docs_no_iris = filter_deep_remove_iris_from_list(
            lst=docs, iri_prefixes=TWA_ABOX_PREFIXES + OM_MEASURE_ABOX_PREFIX
        )
        flattened_docs = deep_pd_json_normalize_list(docs_no_iris)
        table_data = TableData.from_data(flattened_docs)
        logger.info("Done")

        data_artifact = table_data
        support_data: list[DataItem] = [docs_collection, table_data]
        return support_data, data_artifact, vis_var2iris


@cache
def get_sparqlReq_executor(
    ns2endpoint: Annotated[dict[str, str], Depends(get_ns2endpoint)],
    query_processor: Annotated[
        SparqlQueryProcessor, Depends(get_sparqlQuery_processor)
    ],
    response_processor: Annotated[
        SparqlResponseTransformer, Depends(get_sparqlRes_transformer)
    ],
):
    return SparqlDataReqExecutor(
        ns2endpoint=ns2endpoint,
        query_processor=query_processor,
        response_processor=response_processor,
    )
