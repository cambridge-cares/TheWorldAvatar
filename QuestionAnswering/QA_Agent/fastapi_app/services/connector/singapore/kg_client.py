from functools import cache
import os

from services.kg_client import KgClient


@cache
def get_singapore_kg_client():
    return KgClient(os.getenv("KG_ENDPOINT_SINGAPORE", "localhost"))