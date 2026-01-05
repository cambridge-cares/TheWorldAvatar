from functools import cache
import logging
from typing import Annotated
import requests

from fastapi import Depends
from rdflib import SKOS
from constants.namespace import ONTOSPECIES
from model.entity_linking.ontospecies import ElementLinkingArgs, SpeciesLinkingArgs
from services.sparql import (SparqlClient,
    get_ontospecies_endpoint, get_ontospecies_endpoint_v3,
    get_pubchem_agent_url
)
from .base import LinkerManager


logger = logging.getLogger(__name__)


class OntospeciesLinkerManager(LinkerManager):
    SPECIES_IDENTIFIER_KEY_TO_PREDICATE_KEY = {
        "inchi": "InChI",
        "smiles": "SMILES",
        "iupac_name": "IUPACName",
    }

    # TODO: ONTOSPECIES_V3 should be remove after merging
    def __init__(self, ontospecies_endpoint: str,
        ontospecies_endpoint_v3: str, pubchem_agent_url: str
    ):
        self.sparql_client = SparqlClient(ontospecies_endpoint)
        self.sparql_client_v3 = SparqlClient(ontospecies_endpoint_v3)
        self.pubchem_agent_url = pubchem_agent_url

    @property
    def cls2linker(self):
        return {"os:Species": self.linkSpecies, "pt:Element": self.linkElement}

    def linkSpecies(self, text: str | None, **kwargs):
        args = SpeciesLinkingArgs.model_validate(kwargs)
        kvs = [
            ("InChI", args.inchi),
            ("SMILES", args.smiles),
            ("IUPACName", args.iupac_name),
        ]
        try:
            key, val = next((k, v) for k, v in kvs if v)
            logger.info(f"Linking species with args: {key}={val}")
            query = f"""PREFIX os: <{ONTOSPECIES}>

SELECT ?Species
WHERE {{
    ?Species a os:Species .
    ?Species os:has{key} ?Identifier.
    ?Identifier os:value | rdfs:label "{val}".
}}"""
            # TODO: ONTOSPECIES_V3 should be remove after merging
            _, bindings = self.sparql_client.querySelectThenFlatten(query)
            _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
            bindings += bindings_v3
            iris = [row["Species"] for row in bindings]

            if iris:
                logger.info(f"Linked IRIs: {iris}")
                return iris
            else:
                # We were not able to link the species with an IRI.
                logger.info("Unable to link species to IRI.")
                # If we have an InChI...
                if key == "InChI":
                    # ...call the PubChem agent.
                    logger.info(f"Calling PubChem agent at '{self.pubchem_agent_url}'...")
                    pca_response = requests.get(self.pubchem_agent_url,
                        params={"inchi": val})
                    pca_resp_str = pca_response.text.strip()
                    logger.info(f"Response: {pca_resp_str}")
                    if pca_resp_str.startswith("http"):
                        iris = [pca_resp_str]
                        logger.info(f"Linked IRIs: {iris}")
                        return iris
        except StopIteration:
            pass

        texts = list(kwargs.values())
        if (text is not None) and (text != ""):
            texts.append(text)
        if not texts:
            lst: list[str] = []
            return lst

        logger.info(f"Linking species by matching with all identifiers: {texts}")

        query = """PREFIX skos: <{skos}>
PREFIX os: <{os}>

SELECT ?Species WHERE {{
    ?Species a os:Species .
    VALUES ?LCText {{ {values} }}
    ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES|os:hasIdentifier)/os:value)|rdfs:label|skos:altLabel) ?Text .
    BIND(LCASE(?Text) AS ?LCText)
}}""".format(
            skos=SKOS,
            os=ONTOSPECIES,
            values=" ".join('"{val}"'.format(val=text.lower()) for text in texts),
        )

        # TODO: ONTOSPECIES_V3 should be remove after merging
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        _, bindings_v3 = self.sparql_client_v3.querySelectThenFlatten(query)
        bindings += bindings_v3
        iris = [row["Species"] for row in bindings]
        return iris

    def linkElement(self, text: str | None, **kwargs):
        try:
            args = ElementLinkingArgs.model_validate(kwargs)
        except:
            lst: list[str] = []
            return lst

        logger.info(f"Linking elements with args: {args.model_dump_json()}")

        query = f"""PREFIX os: <{ONTOSPECIES}>
        
SELECT DISTINCT *
WHERE {{
    ?Element os:hasElementSymbol/os:value "{args.symbol}"
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)
        iris = [binding["Element"] for binding in bindings]
        return iris


# TODO: ONTOSPECIES_V3 should be remove after merging
@cache
def get_ontospecies_linkerManager(
    ontospecies_endpoint: Annotated[str, Depends(get_ontospecies_endpoint)],
    ontospecies_endpoint_v3: Annotated[str, Depends(get_ontospecies_endpoint_v3)],
    pubchem_agent_url: Annotated[str, Depends(get_pubchem_agent_url)]
):
    return OntospeciesLinkerManager(ontospecies_endpoint=ontospecies_endpoint, 
                                    ontospecies_endpoint_v3=ontospecies_endpoint_v3,
                                    pubchem_agent_url=pubchem_agent_url)
