from functools import cache
import logging
from typing import Annotated, Any

from fastapi import Depends

from .endpoints import get_ns2endpoint


logger = logging.getLogger(__name__)


class SparqlQueryProcessor:
    def __init__(self, ns2endpoint: dict[str, str] = dict()):
        self.ns2endpoint = ns2endpoint

    def inject_service_endpoint(self, sparql: str):
        idx = 0

        while idx < len(sparql):
            start = sparql.find("SERVICE", idx)
            if start < 0:
                break

            logger.info("Found SERVICE keyword")

            start += len("SERVICE")
            while start < len(sparql) and sparql[start].isspace():
                start += 1
            if start >= len(sparql) or sparql[start] != "<":
                break

            end = sparql.find(">", start)
            if end < 0:
                break

            ns = sparql[start + 1 : end]

            logger.info("Found namespace: " + ns)

            if ns in self.ns2endpoint:
                logger.info("Namespace URI: " + self.ns2endpoint[ns])
                sparql = "{before}<{uri}>{after}".format(
                    before=sparql[:start],
                    uri=self.ns2endpoint[ns],
                    after=sparql[end + 1 :],
                )
                idx = start + len(self.ns2endpoint[ns]) + 1
            else:
                logger.info("Namespace URI not found")
                idx = end + 1

        return sparql

    def inject_bindings(
        self,
        sparql: str,
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, Any],
    ):
        values_clauses = [
            "VALUES ?{var} {{ {iris} }}".format(
                var=var, iris=" ".join("<{iri}>".format(iri=iri) for iri in iris)
            )
            for var, iris in entity_bindings.items()
        ]

        if values_clauses:
            open_brace_idx = sparql.find(
                "{"
            )  # assumed to be the start of WHERE patterns

            return "{before}{{\n  {values_clauses}{after}".format(
                before=sparql[:open_brace_idx],
                values_clauses="\n  ".join(values_clauses),
                after=sparql[open_brace_idx + 1 :],
            )
        else:
            return sparql

    def process(
        self,
        sparql: str,
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, Any],
    ):
        logger.info("Processing SPARQL query...")

        sparql = self.inject_service_endpoint(sparql)
        return self.inject_bindings(
            sparql=sparql,
            entity_bindings=entity_bindings,
            const_bindings=const_bindings,
        )


@cache
def get_sparqlQuery_processor(
    ns2endpoint: Annotated[dict[str, str], Depends(get_ns2endpoint)],
):
    return SparqlQueryProcessor(ns2endpoint=ns2endpoint)
