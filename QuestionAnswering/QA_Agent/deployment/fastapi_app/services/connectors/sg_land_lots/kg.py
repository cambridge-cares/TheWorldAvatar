from functools import cache
import os

from services.core.kg import KgClient


@cache
def get_sgLandLots_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS", "localhost"))
