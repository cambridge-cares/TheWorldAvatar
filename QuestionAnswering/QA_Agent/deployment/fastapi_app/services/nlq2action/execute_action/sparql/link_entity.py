from functools import cache
import logging
from typing import Annotated

from fastapi import Depends

from services.nlq2action.link_entity import ELMediator, get_el_mediator


logger = logging.getLogger(__name__)


class SparqlEntityLinker:
    def __init__(self, el_mediator: ELMediator):
        self.el_mediator = el_mediator

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
            iris = self.el_mediator.link(entity_type, surface_form)
        except Exception as e:
            logger.error("Error during entity linking: " + str(e))
            return [token]

        return ["<{iri}>".format(iri=iri) for iri in iris]


@cache
def get_sparql_entityLinker(
    el_mediator: Annotated[ELMediator, Depends(get_el_mediator)]
):
    return SparqlEntityLinker(el_mediator)
