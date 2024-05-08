from functools import cache
import logging
from typing import Annotated, Dict, List

from fastapi import Depends

from services.kg import KgClient
from utils.collections import FrozenDict
from services.entity_store import EntityStore, get_entity_store
from .kg import get_ns2kg


logger = logging.getLogger(__name__)


class SparqlQueryProcessor:
    def __init__(self, entity_store: EntityStore, ns2uri: Dict[str, str] = dict()):
        self.entity_store = entity_store
        self.ns2uri = ns2uri

    def _link_token(self, token: str):
        logger.info("Performing linking for token: " + token)
        
        if token.startswith("<") and token.endswith(">") and ":" in token:
            clsname, surface_form = token[1:-1].split(":", maxsplit=1)

            if surface_form.startswith('"') and surface_form.endswith('"'):
                surface_form = surface_form[1:-1]
                iris = self.entity_store.link(
                    surface_form=surface_form, clsname=clsname
                )

                logger.info(
                    "Linked IRIs for {token}: {iris}".format(token=token, iris=iris)
                )

                return ["<{iri}>".format(iri=iri) for iri in iris]

        return [token]

    def link_entities(self, sparql: str):
        idx = 0

        while idx < len(sparql):
            # VALUES ?LandUseType { <LandUseType:\"residential\"> }
            values_start = sparql.find("VALUES", idx)
            if values_start < 0:
                break

            logger.info("Found VALUES keyword")

            idx_start = values_start + len("VALUES") + 1
            while idx_start < len(sparql) and sparql[idx_start].isspace():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            # ?LandUseType { <LandUseType:\"residential\"> }
            if sparql[idx_start] != "?":
                break

            idx = idx_start + 1
            while idx < len(sparql) and (sparql[idx].isalnum() or sparql[idx] == "_"):
                idx += 1
            if idx >= len(sparql):
                break

            varnode = sparql[idx_start:idx]
            logger.info("Variable node: " + varnode)

            idx_start = idx
            while idx_start < len(sparql) and sparql[idx_start].isspace():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            # { <LandUseType:\"residential\"> }
            if sparql[idx_start] != "{":
                break

            tokens: List[str] = []

            token_start = idx_start + 1
            while token_start < len(sparql):
                while token_start < len(sparql) and sparql[token_start].isspace():
                    token_start += 1
                if token_start >= len(sparql):
                    break

                if sparql[token_start] == "}":
                    break

                # assume no nested double quotations or pointed brackets
                found = False
                for c_start, c_end in ['""', "<>"]:
                    if sparql[token_start] == c_start:
                        token_end = sparql.find(c_end, token_start + 1)
                        if token_end < 0:
                            continue
                        found = True
                        break

                if not found:
                    token_end = token_start + 1
                    while token_end < len(sparql) and not sparql[token_end].isspace():
                        token_end += 1
                    if token_end >= len(sparql):
                        break

                token = sparql[token_start : token_end + 1]
                logger.info("Extracted argument of VALUES clause: " + token)
                tokens.append(token)

                token_start = token_end + 1

            logger.info("Extracted tokens: " + str(tokens))

            if sparql[token_start] != "}":
                break

            idx_end = token_start

            values = []
            for token in tokens:
                values.extend(self._link_token(token))

            # TODO: remove custom replacement of VALUES clause with FILTER
            # once Karthik fixes the bug that causes ontop to throw error
            # on VALUES of facility IRIs.
            if any(token.startswith("<Facility:") for token in tokens):
                filter_clause = "FILTER ( {varnode} IN ( {values} ) )".format(
                    varnode=varnode, values=", ".join(values)
                )
                sparql = "{before}{filter}{after}".format(
                    before=sparql[:values_start],
                    filter=filter_clause,
                    after=sparql[idx_end + 1 :],
                )
                idx = values_start + len(filter_clause) + 1
            else:
                values_str = " ".join(values)

                sparql = "{before}{{ {values} }}{after}".format(
                    before=sparql[:idx_start],
                    values=values_str,
                    after=sparql[idx_end + 1 :],
                )
                idx = idx_start + 2 + len(values_str) + 2

        return sparql

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

    def process(self, sparql: str):
        logger.info("Process SPARQL query:\n" + sparql)

        sparql = self.inject_service_endpoint(sparql)
        return self.link_entities(sparql)


@cache
def get_sparqlQuery_processor(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    ns2kg: Annotated[FrozenDict[str, KgClient], Depends(get_ns2kg)],
):
    return SparqlQueryProcessor(
        entity_store=entity_store,
        ns2uri={ns: kg.sparql.endpoint for ns, kg in ns2kg.items()},
    )
