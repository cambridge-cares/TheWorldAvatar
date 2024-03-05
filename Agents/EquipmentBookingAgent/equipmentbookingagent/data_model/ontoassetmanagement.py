from __future__ import annotations

import pydantic
from typing import Optional, List

from equipmentbookingagent.data_model.iris import *
from pyderivationagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF

from equipmentbookingagent.data_model.base_ontology import BaseOntology



class BookingSystem(BaseOntology):
    hasBooking: Booking
    clz: str = OAM_BOOKINGSYSTEM

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <BookingSystem> <rdf:type> <ontoassetmanagement:BookingSystem>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <BookingSystem> <ontoassetmanagement:hasBooking> <ontoassetmanagement:Booking>
        g.add((URIRef(self.instance_iri), URIRef(OAM_HASBOOKING), URIRef(self.hasBooking.instance_iri)))

        return g

class Booking(BaseOntology):
    clz: str = OAM_BOOKING
