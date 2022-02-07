from __future__ import annotations

import pydantic
from typing import Optional, List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontodoe import *
from chemistry_and_robots.data_model.ontorxn import *

class ParameterSetting(BaseOntology):
    # Here hasQuantity is pointing to the IRI of the actual instance
    hasQuantity: str

class TemperatureSetting(ParameterSetting):
    pass

class DurationSetting(ParameterSetting):
    pass

class VolumeSetting(ParameterSetting):
    pass

class FlowRateSetting(ParameterSetting):
    pass

class EquipmentSettings(BaseOntology):
    # hasSetting: List[ParameterSetting]

    def create_instance_for_kg(self, g: Graph) -> Graph:
        pass

class PowerSupply(BaseOntology):
    pass

class Laboratory(BaseOntology):
    pass

class Saref_Device(BaseOntology):
    pass

class LabEquipment(Saref_Device):
    manufacturer: str # it should be pointing to an instance of https://dbpedia.org/ontology/Organisation, but we simplified here
    isContainedIn: Laboratory
    hasPowerSupply: PowerSupply
    consistsOf: Optional[List[LabEquipment]] = None
    isSpecifiedBy: Optional[EquipmentSettings] = None
    willBeSpecifiedBy: Optional[List[EquipmentSettings]] = None # TODO add this to TBox
    wasSpecifiedBy: Optional[List[EquipmentSettings]] = None # TODO add this to TBox
    # TODO add support for hasHeight, hasLength, hasPrice, hasWeight, and hasWidth
