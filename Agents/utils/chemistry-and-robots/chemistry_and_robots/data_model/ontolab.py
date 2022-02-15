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
    isContainedIn: Union[str, Laboratory] # NOTE here str is provided as an optional as it seems impossible to circular reference at instance level
    hasPowerSupply: Union[str, PowerSupply] # NOTE TODO here str is provided as an optional to simplify the implementation
    consistsOf: Optional[List[LabEquipment]] = None
    isSpecifiedBy: Optional[EquipmentSettings] = None
    # willBeSpecifiedBy: Optional[List[EquipmentSettings]] = None # TODO remove this if never be used
    # wasSpecifiedBy: Optional[List[EquipmentSettings]] = None # TODO remove this is never be used
    # TODO add support for hasHeight, hasLength, hasPrice, hasWeight, and hasWidth

class PreparationMethod(BaseOntology):
    pass

class OntoCAPE_MaterialAmount(BaseOntology):
    pass

class ChemicalSolution(OntoCAPE_MaterialAmount):
    refersToMaterial: OntoCAPE_Material
    # NOTE "files" should point to the actual instance of ontovapourtec.Vial, but here we simplify it with only pointing to the iri
    # NOTE this is due to practical reason as we need to import ontovapourtec.Vial here, but it will cause circular import issue
    # NOTE str will be used for simplicity until a good way to resolve circular import can be find
    # NOTE update_forward_ref() with 'Vial' annotation won't help as we will need to put ChemicalSolution.update_forward_ref() in ontovapourtec
    # NOTE which won't work as developer will need to also import ontovapourtec to make ChemicalSolution fully resolvable
    # NOTE which defeats the whole point of making them separate
    fills: str
    isPreparedBy: Optional[PreparationMethod] = None
