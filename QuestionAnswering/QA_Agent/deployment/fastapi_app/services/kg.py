from functools import cache
import os

from core.kg import KgClient


@cache
def get_ontospecies_kgClient():
    return KgClient(os.getenv("KG_ENDPOINT_ONTOSPECIES", "localhost"))


@cache
def get_sg_ontopClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_ONTOP"))


@cache
def get_sgPlot_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_PLOT", "localhost"))


@cache
def get_sgCompany_bgClient():
    return KgClient(
        os.getenv("KG_ENDPOINT_SG_COMPANY", "localhost"),
    )


@cache
def get_sgDispersion_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_DISPERSION"))


@cache
def get_sgCarpark_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_CARPARKS"))
