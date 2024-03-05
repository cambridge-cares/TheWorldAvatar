# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
from datetime import datetime
from typing import Tuple
from rdflib import Graph
from rdflib import XSD
from pathlib import Path
import collections
import requests
from requests import status_codes
import uuid
import time
import os

from pyderivationagent.kg_operations import PySparqlClient
from pyderivationagent.data_model import *

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


class EquipmentBookingSparqlClient(PySparqlClient):

    def get_all_chemical_reaction_iri(self):
        query = f"""SELECT ?chem_rxn WHERE {{ ?chem_rxn a <{ONTOREACTION_CHEMICALREACTION}>. }}"""
        response = self.performQuery(query)
        return [list(res.values())[0] for res in response]


    def create_chemical_amount_for_outlet_to_waste(self, waste_bottle_iri: str, amount_of_chemical_amount: float):
        g = Graph()
        waste_bottle_iri = trimIRI(waste_bottle_iri)
        chemical_amount_iri = initialiseInstanceIRI(getNameSpace(waste_bottle_iri), ONTOLAB_CHEMICALAMOUNT)
        g.add((URIRef(chemical_amount_iri), RDF.type, URIRef(ONTOLAB_CHEMICALAMOUNT)))
        update = f"""{PREFIX_RDF} INSERT DATA {{
            <{chemical_amount_iri}> rdf:type <{ONTOLAB_CHEMICALAMOUNT}>.
            <{waste_bottle_iri}> <{ONTOLAB_ISFILLEDWITH}> <{chemical_amount_iri}>.}}"""
        self.performUpdate(update)
        self.update_waste_bottle_liquid_level_millilitre(
            {waste_bottle_iri:amount_of_chemical_amount}, for_consumption=False
        )
        return g