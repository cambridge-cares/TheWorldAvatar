from functools import cache
from typing import Annotated, Dict

from fastapi import Depends

from services.nlq2action.link_entity.link import EntityIRI, EntityLabel

from .link_value import SparqlEntityLinker, get_sparql_entityLinker


class SparqlPostProcessor:
    def __init__(self, entity_linker: SparqlEntityLinker):
        self.entity_linker = entity_linker

    def postprocess(self, sparql: str):
        varname2iri2label: Dict[str, Dict[EntityIRI, EntityLabel]] = dict()

        idx = 0
        while idx < len(sparql):
            # VALUES ?LandUseType { <LandUseType:\"residential\"> }
            idx_start = sparql.find("VALUES", idx)
            if idx_start < 0:
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
            idx = idx_start + 1
            while idx < len(sparql) and sparql[idx].isalnum():
                idx += 1
            if idx >= len(sparql):
                break
            varname = sparql[idx_start:idx]
            idx_start = idx

            while idx_start < len(sparql) and sparql[idx_start].isspace():
                idx_start += 1
            if idx_start >= len(sparql):
                break

            # { <LandUseType:\"residential\"> }
            if sparql[idx_start] != "{":
                break

            tokens = []

            token_start = idx_start + 1
            while True:
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
                        tokens.append(sparql[token_start : token_end + 1])
                        token_start = token_end + 1
                        break

                if not found:
                    token_end = token_start + 1
                    while token_end < len(sparql) and not sparql[token_end].isspace():
                        token_end += 1
                    if token_end >= len(sparql):
                        break
                    tokens.append(sparql[token_start : token_end + 1])
                    token_start = token_end + 1

            if sparql[token_start] != "}":
                break
            idx_end = token_start

            values = []
            iri2label = dict()
            for token in tokens:
                _values, _iri2label = self.entity_linker.link(token)
                values.extend(_values)
                iri2label.update(_iri2label)

            values = " ".join(values)
            varname2iri2label[varname] = iri2label

            sparql = "{before}{{ {values} }}{after}".format(
                before=sparql[:idx_start], values=values, after=sparql[idx_end + 1 :]
            )
            idx = idx_start + 2 + len(values) + 2

        return sparql, varname2iri2label


@cache
def get_sparql_postprocessor(
    entity_linker: Annotated[SparqlEntityLinker, Depends(get_sparql_entityLinker)]
):
    return SparqlPostProcessor(entity_linker)
