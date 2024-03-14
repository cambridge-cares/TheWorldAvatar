from functools import cache
import os

from services.kg_client import KgClient


@cache
def get_singapore_bg_client():
    return KgClient(os.getenv("KG_ENDPOINT_SINGAPORE", "localhost"))

@cache
def get_singapore_ontop_client():
    return KgClient(os.getenv("KG_ENDPOINT_SINGAPORE_ONTOP", "localhost"))