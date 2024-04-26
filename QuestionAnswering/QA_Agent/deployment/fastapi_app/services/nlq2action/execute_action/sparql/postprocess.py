import logging
from typing import List

from services.entity_store import EntityStore


logger = logging.getLogger(__name__)


class SparqlPostProcessor:
    def __init__(self, entity_linker: EntityStore):
        self.entity_linker = entity_linker

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
            iris = self.entity_linker.link(entity_type, surface_form)
        except Exception as e:
            logger.error("Error during entity linking: " + str(e))
            return [token]

        return ["<{iri}>".format(iri=iri) for iri in iris]

    def postprocess(self, sparql: str):
        varnames: List[str] = []

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
            for token in tokens:
                _values = self.entity_linker.link(token)
                values.extend(_values)

            values = " ".join(values)
            varnames.append(varname)

            sparql = "{before}{{ {values} }}{after}".format(
                before=sparql[:idx_start], values=values, after=sparql[idx_end + 1 :]
            )
            idx = idx_start + 2 + len(values) + 2

        return sparql, varnames
