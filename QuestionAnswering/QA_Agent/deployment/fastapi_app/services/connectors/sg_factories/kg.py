from functools import cache
import os

from services.core.kg import KgClient


@cache
def get_sgFactories_bgClient():
    return KgClient(
        os.getenv("KG_ENDPOINT_SG_FACTORIES", "localhost"),
        user=os.getenv("KG_ENDPOINT_SG_FACTORIES_USER"),
        password=os.getenv("KG_ENDPOINT_SG_FACTORIES_PASSWORD"),
    )
