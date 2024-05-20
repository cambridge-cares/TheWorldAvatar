from functools import cache
from typing import Annotated

from fastapi import Depends

from services.kg import KgClient, get_ontospecies_kgClient
from services.kg import get_sg_ontopClient, get_sgCompany_bgClient, get_sgPlot_bgClient
from utils.collections import FrozenDict


@cache
def get_ns2kg(
    ontospecies_client: Annotated[KgClient, Depends(get_ontospecies_kgClient)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    plot_client: Annotated[KgClient, Depends(get_sgPlot_bgClient)],
    company_client: Annotated[KgClient, Depends(get_sgCompany_bgClient)],
):
    return FrozenDict(
        {
            "ontospecies": ontospecies_client,
            "ontop": ontop_client,
            "plot": plot_client,
            "company": company_client,
        }
    )
