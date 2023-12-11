import os

from flask import Flask, g

from .kg_client import KgClient


class KgExecutor:
    def __init__(self):
        ontospecies_endpoint = os.getenv("ONTOSPECIES_ENDPOINT")
        print(
            "Initialize KG client for OntoSpecies with the endpoint ",
            ontospecies_endpoint,
        )
        ontospecies_client = KgClient(ontospecies_endpoint)

        ontokin_endpoint = os.getenv("ONTOKIN_ENDPOINT")
        print("Initialize KG client for OntoKin with the endpoint ", ontokin_endpoint)
        ontokin_client = KgClient(
            ontokin_endpoint,
            user=os.getenv("ONTOKIN_USERNAME"),
            pw=os.getenv("ONTOKIN_PASSWORD"),
        )

        ontocompchem_endpoint = os.getenv("ONTOCOMPCHEM_ENDPOINT")
        print(
            "Initialize KG client for OntoCompChem with the endpoint ",
            ontocompchem_endpoint,
        )
        ontocompchem_client = KgClient(ontocompchem_endpoint)

        self.domain2sparql = dict(
            ontospecies=ontospecies_client,
            ontokin=ontokin_client,
            ontocompchem=ontocompchem_client,
        )

    def get_domains(self):
        return list(self.domain2sparql.keys())

    def query(self, domain: str, query: str):
        # TODO: align predicted domain to actual domain based on Levenshtein distance
        return self.domain2sparql[domain].query(query)


def get_kg():
    if "kg" not in g:
        g.kg = KgExecutor()
    return g.kg


def close_kg(e = None):
    g.pop("kg", None)


def init_app(app: Flask):
    app.teardown_appcontext(close_kg)
