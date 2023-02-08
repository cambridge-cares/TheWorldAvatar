from __future__ import annotations

import pydantic
from typing import Optional, List

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontodoe import *
from chemistry_and_robots.data_model.ontoreaction import *


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

    def get_parameter_settings(self) -> List[ParameterSetting]:
        return [getattr(self, name) for name in self.__fields__ if isinstance(getattr(self, name), ParameterSetting)]

    def create_instance_for_kg(self, g: Graph, configure_digital_twin: bool) -> Graph:
        # check if information is complete
        if self.specifies == None:
            raise Exception("LabEquipment is not specified when writing triples about <%s> to the KG." % self.instance_iri)
        if self.wasGeneratedFor == None:
            raise Exception("ReactionExperiment is not provided when writing triples about <%s> to the KG." % self.instance_iri)

        # <equip_settings> <rdf:type> <OntoLab:EquipmentSettings>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))
        # <equip_settings> <OntoLab:wasGeneratedFor> <rxnexp>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_WASGENERATEDFOR), URIRef(self.wasGeneratedFor)))
        # <reactionExperiment> <OntoReaction:hasEquipmentSettings> <reactorSetting>
        g.add((URIRef(self.wasGeneratedFor), URIRef(ONTOREACTION_HASEQUIPMENTSETTINGS), URIRef(self.instance_iri)))

        # NOTE the links between <equip_settings> and <lab_equipment> are optional depends on if one want to configure the digital twin
        if configure_digital_twin:
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

class Saref_State(BaseOntology):
    clz: str = SAREF_STATE

class LabEquipment(Saref_Device):
    clz: str = ONTOLAB_LABEQUIPMENT
    manufacturer: str # it should be pointing to an instance of https://dbpedia.org/ontology/Organisation, but we simplified here
    isContainedIn: Union[str, Laboratory] # NOTE here str is provided as an optional as it seems impossible to circular reference at instance level
    hasPowerSupply: Union[str, PowerSupply] # NOTE TODO [future work] here str is provided as an optional to simplify the implementation
    consistsOf: Optional[List[LabEquipment]] = None
    isSpecifiedBy: Optional[EquipmentSettings] = None
    # TODO [future work] add support for hasHeight, hasLength, hasPrice, hasWeight, and hasWidth
    isManagedBy: Optional[str] # NOTE here str is provided, this should refer to the iri of agent service

    def consists_of_lab_equipment(self, lab_equipment_iris: List[str]) -> bool:
        return bool([le for le in self.consistsOf if le.instance_iri in lab_equipment_iris])

class PreparationMethod(BaseOntology):
    clz: str = ONTOLAB_PREPARATIONMETHOD

    def create_instance_for_kg(self, g: Graph) -> Graph:
        raise NotImplementedError("create_instance_for_kg for OntoLab:PreparationMethod is NOT yet implemented.")

class OntoCAPE_MaterialAmount(BaseOntology):
    clz: str = ONTOCAPE_MATERIALAMOUNT

class ChemicalSolution(OntoCAPE_MaterialAmount):
    clz: str = ONTOLAB_CHEMICALSOLUTION
    refersToMaterial: Optional[OntoCAPE_Material] # NOTE OntoCAPE_Material is made optional to accommodate the situation where ChemicalSolution is generated but not characterised yet, i.e. unknow concentration
    # NOTE "fills" should point to the actual instance of ontovapourtec.Vial, but here we simplify it with only pointing to the iri
    # NOTE this is due to practical reason as we need to import ontovapourtec.Vial here, but it will cause circular import issue
    # NOTE str will be used for simplicity until a good way to resolve circular import can be find
    # NOTE update_forward_ref() with 'Vial' annotation won't help as we will need to put ChemicalSolution.update_forward_ref() in ontovapourtec
    # NOTE which won't work as developer will need to also import ontovapourtec to make ChemicalSolution fully resolvable
    # NOTE which defeats the whole point of making them separate
    # NOTE "fills" should also be able to point to actual instance of ontolab.ReagentBottle
    # TODO [future work] provide super class for ontovapourtec.Vial and ontolab.ReagentBottle, e.g. ontolab.Container
    fills: str
    isPreparedBy: Optional[PreparationMethod] = None
    containsUnidentifiedComponent: Optional[bool] = False

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <chemical_solution> <rdf:type> <ChemicalSolution>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(self.clz)))

        # <chemical_solution> <fills> <vial>
        g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_FILLS), URIRef(self.fills)))

        # <chemical_solution> <refersToMaterial> <material>
        g.add((URIRef(self.instance_iri), URIRef(ONTOCAPE_REFERSTOMATERIAL), URIRef(self.refersToMaterial.instance_iri)))
        g = self.refersToMaterial.create_instance_for_kg(g)

        if self.isPreparedBy is not None:
            g = self.isPreparedBy.create_instance_for_kg(g)

        if self.containsUnidentifiedComponent is not None:
            # NOTE only add this triple when it's known, otherwise just skip it
            # <chemical_solution> <containsUnidentifiedComponent> boolean
            g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_CONTAINSUNIDENTIFIEDCOMPONENT), Literal(self.containsUnidentifiedComponent)))

        return g

class ReagentBottle(BaseOntology):
    clz: str = ONTOLAB_REAGENTBOTTLE
    isFilledWith: ChemicalSolution
    hasFillLevel: OM_Volume
    hasWarningLevel: OM_Volume
    hasMaxLevel: OM_Volume

    def contains_chemical_species(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
        species_to_exclude: List[str]=None,
    ) -> bool:
        if self.isFilledWith is not None:
            if self.isFilledWith.refersToMaterial is not None:
                if self.isFilledWith.refersToMaterial.contains_chemical_species(solute, solvent_as_constraint, species_to_exclude):
                    return True
        return False
