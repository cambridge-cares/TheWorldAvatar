import pydantic
from typing import List

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontolab import *

class SampleLoopVolumeSetting(VolumeSetting):
    pass

class ReactorTemperatureSetting(TemperatureSetting):
    clz: str = ONTOVAPOURTEC_REACTORTEMPERATURESETTING

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <reactorTempSetting> <rdf:type> <OntoVapourtec:ReactorTemperatureSetting>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOVAPOURTEC_REACTORTEMPERATURESETTING)))
        # <reactorTempSetting> <OntoLab:hasQuantiry> <quantity>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASQUANTITY), URIRef(self.hasQuantity)))

        return g

class ResidenceTimeSetting(DurationSetting):
    clz: str = ONTOVAPOURTEC_RESIDENCETIMESETTING

    def create_instance_for_kg(self, g: Graph) -> Graph:
        # <resTimeSetting> <rdf:type> <OntoVapourtec:ResidenceTimeSetting>
        g.add((URIRef(self.instance_iri), RDF.type, URIRef(ONTOVAPOURTEC_RESIDENCETIMESETTING)))
        # <resTimeSetting> <OntoLab:hasQuantiry> <quantity>
        g.add((URIRef(self.instance_iri), URIRef(ONTOLAB_HASQUANTITY), URIRef(self.hasQuantity)))

        return g

class ReactorSettings(EquipmentSettings):
    hasResidenceTimeSetting: ResidenceTimeSetting
    hasReactorTemperatureSetting: ReactorTemperatureSetting
    clz: str = ONTOVAPOURTEC_REACTORSETTING

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

class PumpSettings(EquipmentSettings):
    hasFlowRateSetting: FlowRateSetting
    hasSampleLoopVolumeSetting: SampleLoopVolumeSetting
    # Here pumpsLiquidFrom is kept as str for simplicity for now
    pumpsLiquidFrom: str
    clz: str = ONTOVAPOURTEC_PUMPSETTINGS

    def create_instance_for_kg(self, g: Graph) -> Graph:
        pass
