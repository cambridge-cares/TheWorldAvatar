from functools import cache
import os
from typing import Annotated
from fastapi import Depends
import requests


class FeatureInfoClient:
    def __init__(self, url: str):
        self.url = url

    def query(self, iri: str):
        res = requests.get(self.url, params=dict(iri=iri))
        res.raise_for_status()
        return res.json()


@cache
def get_featureInfoAgentUrl():
    return os.getenv("ENDPOINT_FEATURE_INFO_AGENT")


@cache
def get_featureInfoClient(url: Annotated[str, Depends(get_featureInfoAgentUrl)]):
    return FeatureInfoClient(url)
