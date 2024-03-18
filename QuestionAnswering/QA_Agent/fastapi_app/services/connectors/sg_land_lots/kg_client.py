from functools import cache
import os

from services.kg_client import KgClient


@cache
def get_sg_land_lots_bg_client():
    return KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS", "localhost"))

@cache
def get_sg_land_lots_ontop_client():
    return KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS_ONTOP", "localhost"))