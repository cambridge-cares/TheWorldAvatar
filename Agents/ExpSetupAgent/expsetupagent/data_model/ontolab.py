import pydantic
from pydantic.dataclasses import dataclass
from typing import Optional, List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from rdflib import Graph, URIRef, Namespace, Literal, BNode

from src.data_model.ontodoe import *

@dataclass
class ParameterSetting:
    instance_iri: str
    # Here hasQuantity is pointing to the IRI of the actual instance
    hasQuantity: str
    namespace_for_init: Optional[str] = None

@dataclass
class TemperatureSetting(ParameterSetting):
    pass

@dataclass
class DurationSetting(ParameterSetting):
    pass

@dataclass
class VolumeSetting(ParameterSetting):
    pass

@dataclass
class FlowRateSetting(ParameterSetting):
    pass

@dataclass
class EquipmentSettings():
    instance_iri: str
    # hasSetting: List[ParameterSetting]

    def create_instance_for_kg(self, g: Graph) -> Graph:
        pass

# @dataclass
# class NewEquipSettings():
#     instance_iri: str
#     isGeneratedFor: ReactionExperiment
#     isGeneratedAt: int
#     hasEquipmentSettings: List[EquipmentSettings]
#     namespace_for_init: Optional[str] = None

#     def __post_init__(self):
#         if self.instance_iri == INSTANCE_IRI_TO_BE_INITIALISED:
#             if self.namespace_for_init is not None:
#                 self.instance_iri = initialiseInstanceIRI(self.namespace_for_init, ONTOLAB_NEWEQUIPSETTINGS)
#             else:
#                 raise Exception(f"A namespace should be provided for initialising a/an {self.__class__} instance.")

#     def create_instance_for_kg(self, g: Graph) -> Graph:
#         # Add triples:
#         # <dtconf> <rdf:type> <OntoLab:DigitalTwinConfig>
#         g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOLAB_NEWEQUIPSETTINGS)))

#         # <dtconf> <OntoLab:isGeneratedFor> <rxnexp/rxnvar>
#         g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_ISGENERATEDFOR), URIRef(self.isGeneratedFor.instance_iri)))

#         # <dtconf> <OntoLab:isGeneratedAt> 123
#         g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_ISGENERATEDAT), Literal(self.isGeneratedAt)))

#         # Iterate over the list of <OntoLab:EquipmentSettings>
#         for equip_set in self.hasEquipmentSettings:
#             # <dtconf> <OntoLab:hasEquipmentSettings> <equipSetting>
#             g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASEQUIPMENTSETTINGS), URIRef(equip_set.instance_iri)))
#             g = equip_set.create_instance_for_kg(g)
        
#         return g
