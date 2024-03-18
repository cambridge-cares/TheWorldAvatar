from functools import cache
import os

from services.kg_client import KgClient


@cache
def get_ontospecies_kg_client():
    return KgClient(os.getenv("KG_ENDPOINT_ONTOSPECIES", "localhost"))