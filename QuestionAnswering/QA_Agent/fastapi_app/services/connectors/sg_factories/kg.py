from functools import cache
import os

from services.core.kg import KgClient

@cache
def get_sgFactories_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_FACTORIES", "localhost"))

@cache
def get_sgFactories_ontopClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_FACTORIES_ONTOP", "localhost"))
