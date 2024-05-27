from functools import cache
from typing import Annotated

from fastapi import Depends

from services.processs_response.expand_response import SparqlResponseExpander
from services.processs_response.ontocompchem import get_ontocompchem_responseExpander
from services.processs_response.ontokin import get_ontokin_responseExpander


@cache
def get_response_expanders(
    ontokin_expander: Annotated[
        SparqlResponseExpander, Depends(get_ontokin_responseExpander)
    ],
    ontocompchem_expander: Annotated[
        SparqlResponseExpander, Depends(get_ontocompchem_responseExpander)
    ],
):
    return (ontokin_expander, ontocompchem_expander)
