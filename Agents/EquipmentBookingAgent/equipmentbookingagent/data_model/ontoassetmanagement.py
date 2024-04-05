from __future__ import annotations

import pydantic
from typing import Optional, List

from equipmentbookingagent.data_model.iris import *
from pyderivationagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode
from rdflib.namespace import RDF

from equipmentbookingagent.data_model.base_ontology import BaseOntology



class BookingSystem(BaseOntology):
    hasBooking: Optional[Booking] = None
    clz: str = OAM_BOOKINGSYSTEM

class Booking(BaseOntology):
    clz: str = OAM_BOOKING
    hasBooker: Fibo_Person
    hasBookingPeriod: str #TIME_INTERVAL

class Equipment(BaseOntology):
    # can either be bot:Element (of which OntoDevice:Device is a subclass) or ssn:System
    clz: str
    hasBookingSystem: BookingSystem
    label: str
    hasItemInventoryIdentifier: str
    isManufacturedBy: str #FIBO_ORGANIZATION
    isSuppliedBy: str #FIBO_ORGANIZATION
    isLocatedIn: str #ONTOBIM_ROOM

class Fibo_Person(BaseOntology):
    clz : str = FIBO_PERSON
    name : str