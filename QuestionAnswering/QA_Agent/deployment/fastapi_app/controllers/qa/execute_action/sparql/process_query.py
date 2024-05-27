from functools import cache
import logging
from typing import Annotated, Dict, List

from fastapi import Depends

from services.kg import KgClient
from utils.collections import FrozenDict
from .kg import get_ns2kg


logger = logging.getLogger(__name__)


class SparqlQueryProcessor:
    def __init__(self, ns2uri: Dict[str, str] = dict()):
        self.ns2uri = ns2uri

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

            if ns in self.ns2uri:
                logger.info("Namespace URI: " + self.ns2uri[ns])
                sparql = "{before}<{uri}>{after}".format(
                    before=sparql[:start], uri=self.ns2uri[ns], after=sparql[end + 1 :]
                )
                idx = start + len(self.ns2uri[ns]) + 1
            else:
                logger.info("Namespace URI not found")
                idx = end + 1

        return sparql

    def inject_bindings(self, sparql: str, bindings: Dict[str, List[str]]):
        values_clauses = [
            "VALUES {var} {{ {iris} }}".format(
                var=var, iris=" ".join("<{iri}>".format(iri=iri) for iri in iris)
            )
            for var, iris in bindings.items()
        ]

        if values_clauses:
            qn_mark_idx = sparql.find(
                "?"
            )  # assumed to be the first variable in SPARQL's projection
            open_brace_idx = sparql.find(
                "{"
            )  # assumed to be the start of WHERE patterns

            vars = set(sparql[qn_mark_idx:open_brace_idx].strip().split())
            new_vars = [var for var in bindings.keys() if var not in vars]

            return "{before}{new_vars}{between}{{  {values_clauses}{after}".format(
                before=sparql[:qn_mark_idx],
                new_vars=" " + " ".join(new_vars),
                between=sparql[qn_mark_idx:open_brace_idx],
                values_clauses="\n  ".join(values_clauses),
                after=sparql[open_brace_idx + 1 :],
            )
        else:
            return sparql

    def process(self, sparql: str, bindings: Dict[str, List[str]]):
        logger.info("Process SPARQL query:\n" + sparql)

        sparql = self.inject_service_endpoint(sparql)
        return self.inject_bindings(sparql=sparql, bindings=bindings)


@cache
def get_sparqlQuery_processor(
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
):
    return SparqlQueryProcessor(
        ns2uri={ns: kg.sparql.endpoint for ns, kg in ns2kg.items()},
    )
