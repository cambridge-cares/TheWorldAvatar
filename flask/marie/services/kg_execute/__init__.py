import logging
import os

from flask import g

from marie.exceptions import KgConnectionError, MissingKgEndpointError
from .kg_client import KgClient


logger = logging.getLogger(__name__)


class KgExecutor:
    def __init__(self):
        try:
            ontospecies_endpoint = os.getenv("ONTOSPECIES_ENDPOINT")
            if ontospecies_endpoint is None:
                raise MissingKgEndpointError("OntoSpecies")
            logger.info(
                "Initialize KG client for OntoSpecies with the endpoint " +
                ontospecies_endpoint,
            )
            ontospecies_client = KgClient(ontospecies_endpoint)

            ontokin_endpoint = os.getenv("ONTOKIN_ENDPOINT")
            if ontospecies_endpoint is None:
                raise MissingKgEndpointError("OntoKin")
            logger.info("Initialize KG client for OntoKin with the endpoint " + ontokin_endpoint)
            ontokin_client = KgClient(
                ontokin_endpoint,
                user=os.getenv("ONTOKIN_USERNAME"),
                pw=os.getenv("ONTOKIN_PASSWORD"),
            )

            ontocompchem_endpoint = os.getenv("ONTOCOMPCHEM_ENDPOINT")
            if ontospecies_endpoint is None:
                raise MissingKgEndpointError("OntoCompChem")
            logger.info(
                "Initialize KG client for OntoCompChem with the endpoint " +
                ontocompchem_endpoint,
            )
            ontocompchem_client = KgClient(ontocompchem_endpoint)

            self.domain2sparql = dict(
                ontospecies=ontospecies_client,
                ontokin=ontokin_client,
                ontocompchem=ontocompchem_client,
            )
        except MissingKgEndpointError:
            raise KgConnectionError()


    def get_domains(self):
        return list(self.domain2sparql.keys())

    def query(self, domain: str, query: str):
        # TODO: align predicted domain to actual domain based on Levenshtein distance
        return self.domain2sparql[domain].query(query)


def get_kg():
    if "kg" not in g:
        g.kg = KgExecutor()
    return g.kg
