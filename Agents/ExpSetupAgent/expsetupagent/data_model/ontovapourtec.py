from pydantic.dataclasses import dataclass
from typing import List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from expsetupagent.data_model.ontolab import *


@dataclass
class SampleLoopVolumeSetting(VolumeSetting):
    pass

@dataclass
class ReactorTemperatureSetting(TemperatureSetting):

    def __post_init__(self):
        if self.instance_iri == INSTANCE_IRI_TO_BE_INITIALISED:
            if self.namespace_for_init is not None:
                self.instance_iri = initialiseInstanceIRI(self.namespace_for_init, ONTOVAPOURTEC_REACTORTEMPERATURESETTING)
            else:
                raise Exception(f"A namespace should be provided for initialising a/an {self.__class__} instance.")

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <reactorTempSetting> <rdf:type> <OntoVapourtec:ReactorTemperatureSetting>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOVAPOURTEC_REACTORTEMPERATURESETTING)))
        # <reactorTempSetting> <OntoLab:hasQuantiry> <quantity>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASQUANTITY), URIRef(self.hasQuantity)))

        return g

@dataclass
class ResidenceTimeSetting(DurationSetting):

    def __post_init__(self):
        if self.instance_iri == INSTANCE_IRI_TO_BE_INITIALISED:
            if self.namespace_for_init is not None:
                self.instance_iri = initialiseInstanceIRI(self.namespace_for_init, ONTOVAPOURTEC_RESIDENCETIMESETTING)
            else:
                raise Exception(f"A namespace should be provided for initialising a/an {self.__class__} instance.")

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <resTimeSetting> <rdf:type> <OntoVapourtec:ResidenceTimeSetting>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOVAPOURTEC_RESIDENCETIMESETTING)))
        # <resTimeSetting> <OntoLab:hasQuantiry> <quantity>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASQUANTITY), URIRef(self.hasQuantity)))

        return g

@dataclass
class ReactorSettings(EquipmentSettings):
    hasResidenceTimeSetting: ResidenceTimeSetting
    hasReactorTemperatureSetting: ReactorTemperatureSetting
    namespace_for_init: Optional[str] = None

    def __post_init__(self):
        if self.instance_iri == INSTANCE_IRI_TO_BE_INITIALISED:
            if self.namespace_for_init is not None:
                self.instance_iri = initialiseInstanceIRI(self.namespace_for_init, ONTOVAPOURTEC_REACTORSETTING)
            else:
                raise Exception(f"A namespace should be provided for initialising a/an {self.__class__} instance.")

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <reactorSetting> <rdf:type> <OntoVapourtec:ReactorSettings>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOVAPOURTEC_REACTORSETTING)))

        # <reactorSetting> <OntoVapourtec:hasReactorTemperatureSetting> <reactorTempSetting>
        g.add((
            URIRef(self.instance_iri),
            URIRef(ONTOVAPOURTEC_HASREACTORTEMPERATURESETTING),
            URIRef(self.hasReactorTemperatureSetting.instance_iri)
        ))
        g = self.hasReactorTemperatureSetting.create_instance_for_kg(g)

        # <reactorSetting> <OntoVapourtec:hasResidenceTimeSetting> <resTimeSetting>
        g.add((
            URIRef(self.instance_iri),
            URIRef(ONTOVAPOURTEC_HASRESIDENCETIMESETTING),
            URIRef(self.hasResidenceTimeSetting.instance_iri)
        ))
        g = self.hasResidenceTimeSetting.create_instance_for_kg(g)

        return g

@dataclass
class PumpSettings(EquipmentSettings):
    hasFlowRateSetting: FlowRateSetting
    hasSampleLoopVolumeSetting: SampleLoopVolumeSetting
    # Here pumpsLiquidFrom is kept as str for simplicity for now
    pumpsLiquidFrom: str

    def create_instance_for_kg(self, g: Graph) -> Graph:
        pass
