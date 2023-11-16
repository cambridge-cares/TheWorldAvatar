import os

from .kg_client import KgClient


class KgExecutor:
    def __init__(self):
        ontospecies_endpoint = os.getenv("ONTOSPECIES_ENDPOINT")
        print("Initialize KG client for OntoSpecies with the endpoint " + ontospecies_endpoint)
        ontospecies_client = KgClient(ontospecies_endpoint)

        ontokin_endpoint = os.getenv("ONTOKIN_ENDPOINT")
        print("Initialize KG client for OntoKin with the endpoint " + ontokin_endpoint)
        ontokin_client = KgClient(ontokin_endpoint, user=os.getenv("ONTOKIN_USERNAME"), pw=os.getenv("ONTOKIN_PASSWORD"))

        self.domain2sparql = dict(
            ontospecies=ontospecies_client,
            ontokin=ontokin_client
        )

    def get_domains(self):
        return list(self.domain2sparql.keys())

    def query(self, domain: str, query: str):
        # TODO: align predicted domain to actual domain based on Levenshtein distance
        return self.domain2sparql[domain].query(query)