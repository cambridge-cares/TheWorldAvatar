from functools import cache
from typing import Annotated

from fastapi import Depends

from services.sparql import (
    get_ontocompchem_endpoint,
    get_ontokin_endpoint,
    get_ontomops_endpoint,
    get_ontospecies_endpoint,
    get_ontozeolite_endpoint,
)
from services.sparql import (
    get_sgOntop_endpoint,
    get_sgCompany_endpoint,
    get_sgPlot_endpoint,
)
from utils.collections import FrozenDict


@cache
def get_ns2endpoint(
    ontospecies_endpoint: Annotated[str, Depends(get_ontospecies_endpoint)],
    ontokin_endpoint: Annotated[str, Depends(get_ontokin_endpoint)],
    ontocompchem_endpoint: Annotated[str, Depends(get_ontocompchem_endpoint)],
    ontozeolite_endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)],
    ontomops_endpoint: Annotated[str, Depends(get_ontomops_endpoint)],
    ontop_endpoint: Annotated[str, Depends(get_sgOntop_endpoint)],
    plot_endpoint: Annotated[str, Depends(get_sgPlot_endpoint)],
    company_endpoint: Annotated[str, Depends(get_sgCompany_endpoint)],
):
    return FrozenDict.from_dict(
        {
            "ontospecies": ontospecies_endpoint,
            "ontokin": ontokin_endpoint,
            "ontocompchem": ontocompchem_endpoint,
            "ontozeolite": ontozeolite_endpoint,
            "ontomops": ontomops_endpoint,
            "ontop": ontop_endpoint,
            "plot": plot_endpoint,
            "company": company_endpoint,
        }
    )
