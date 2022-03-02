from __future__ import annotations

import pydantic
from typing import Optional, List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontodoe import *
from chemistry_and_robots.data_model.ontorxn import *

# TODO add below IRIs to pyasyncagent.data_model.iris, also TBox CSV/OWL if applicable
ONTOLAB_WASGENERATEDFOR = ONTOLAB + 'wasGeneratedFor'
ONTOLAB_TRANSLATESTOPARAMETERSETTING = ONTOLAB + 'translatesToParameterSetting'

class ParameterSetting(BaseOntology):
    # NOTE Here hasQuantity is pointing to either IRI or the actual instance of om:Quantity
    hasQuantity: Union[str, OM_Quantity]
    clz: str = ONTOLAB_PARAMETERSETTING

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # IRI-ise instance iri of hasQuantity
        quantity_iri = URIRef(self.hasQuantity) if isinstance(self.hasQuantity, str) else URIRef(self.hasQuantity.instance_iri)
        # <paramSetting> <rdf:type> <self.clz>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <paramSetting> <OntoLab:hasQuantity> <quantity>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASQUANTITY), quantity_iri))
        # <quantity> <OntoLab:translatesToParameterSetting> <paramSetting>
        g.add((quantity_iri, URIRef(ONTOLAB_TRANSLATESTOPARAMETERSETTING), URIRef(self.instance_iri)))

        return g

class TemperatureSetting(ParameterSetting):
    clz: str = ONTOLAB_TEMPERATURESETTING

class DurationSetting(ParameterSetting):
    clz: str = ONTOLAB_DURATIONSETTING

class VolumeSetting(ParameterSetting):
    clz: str = ONTOLAB_VOLUMESETTING

class FlowRateSetting(ParameterSetting):
    clz: str = ONTOLAB_FLOWRATESETTING

class EquipmentSettings(BaseOntology):
    # hasSetting: List[ParameterSetting]
    clz: str = ONTOLAB_EQUIPMENTSETTINGS
    specifies: Optional[LabEquipment]
    wasGeneratedFor: Optional[str] # NOTE here we provided str to simplify the implementation, it should be ReactionExperiment in practice

    @pydantic.root_validator
    @classmethod
    def if_specifies_equipment_for_init(cls, values):
        if 'namespace_for_init' in values and 'specifies' not in values:
            raise Exception("LabEquipment is not specified when creating new instances of EquipmentSettings.")

        if 'namespace_for_init' in values and 'wasGeneratedFor' not in values:
            raise Exception("ReactionExperiment is not specified when creating new instances of EquipmentSettings.")

        return values

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # check if information is complete
        if self.specifies == None:
            raise Exception("LabEquipment is not specified when writing triples about <%s> to the KG." % self.instance_iri)
        if self.wasGeneratedFor == None:
            raise Exception("ReactionExperiment is not provided when writing triples about <%s> to the KG." % self.instance_iri)

        # <equip_settings> <rdf:type> <OntoLab:EquipmentSettings>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <equip_settings> <OntoLab:wasGeneratedFor> <rxnexp>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_WASGENERATEDFOR), URIRef(self.wasGeneratedFor)))
        # <reactionExperiment> <OntoRxn:hasEquipmentSettings> <reactorSetting>
        g.add((URIRef(self.wasGeneratedFor), URIRef(ONTORXN_HASEQUIPMENTSETTINGS), URIRef(self.instance_iri)))

        # <equip_settings> <OntoLab:specifies> <lab_equipment>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_SPECIFIES), URIRef(self.specifies.instance_iri)))
        # <lab_equipment> <OntoLab:isSpecifiedBy> <equip_settings>
        g.add((URIRef(self.specifies.instance_iri), URIRef(ONTOLAB_ISSPECIFIEDBY), URIRef(self.instance_iri)))

        return g

class PowerSupply(BaseOntology):
    pass

class Laboratory(BaseOntology):
    pass

class Saref_Device(BaseOntology):
    clz: str = SAREF_DEVICE

class LabEquipment(Saref_Device):
    clz: str = ONTOLAB_LABEQUIPMENT
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
    clz: str = ONTOCAPE_MATERIALAMOUNT

class ChemicalSolution(OntoCAPE_MaterialAmount):
    clz: str = ONTOLAB_CHEMICALSOLUTION
    refersToMaterial: OntoCAPE_Material
    # NOTE "files" should point to the actual instance of ontovapourtec.Vial, but here we simplify it with only pointing to the iri
    # NOTE this is due to practical reason as we need to import ontovapourtec.Vial here, but it will cause circular import issue
    # NOTE str will be used for simplicity until a good way to resolve circular import can be find
    # NOTE update_forward_ref() with 'Vial' annotation won't help as we will need to put ChemicalSolution.update_forward_ref() in ontovapourtec
    # NOTE which won't work as developer will need to also import ontovapourtec to make ChemicalSolution fully resolvable
    # NOTE which defeats the whole point of making them separate
    fills: str
    isPreparedBy: Optional[PreparationMethod] = None
