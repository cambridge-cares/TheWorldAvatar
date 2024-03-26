from functools import cache
import os
from fastapi_app.services.core.kg import KgClient


@cache
def get_sgDataCentres_ontopClient():
    return KgClient(os.getenv("KG_ENDPOINT_SG_DATA_CENTRES_ONTOP"), "localhost")