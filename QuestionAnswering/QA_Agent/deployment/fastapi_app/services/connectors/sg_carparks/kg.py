from functools import cache
import os

from core.kg import KgClient


@cache
def get_sgCarparks_bgClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_CARPARKS"))
