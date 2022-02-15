import pydantic
from typing import List, Union

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import *
from chemistry_and_robots.data_model.ontolab import *

# TODO add below IRIs to pyasyncagent.data_model.iris, also TBox CSV/OWL if applicable
# TODO NOTE ONTOVAPOURTEC_HASWARNINGLEVEL is probably not needed?
ONTOVAPOURTEC_LOCATIONID = ONTOVAPOURTEC + 'locationID'
ONTOVAPOURTEC_HASREACTORTEMPERATUREUPPERLIMIT = ONTOVAPOURTEC + 'hasReactorTemperatureUpperLimit'
ONTOVAPOURTEC_HASREACTORTEMPERATURELOWERLIMIT = ONTOVAPOURTEC + 'hasReactorTemperatureLowerLimit'
ONTOVAPOURTEC_CONDUCTED = ONTOVAPOURTEC + 'conducted'

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

class Vial(BaseOntology):
    clz: str = ONTOVAPOURTEC_VIAL
    isFilledWith: ChemicalSolution
    hasFillLevel: OM_Volume
    # hasWarningLevel: OM_Volume # NOTE hasWarningLevel is temporarily commented out before a decision is made whether keep it
    hasMaxLevel: OM_Volume
    isHeldIn: str # NOTE here we simplify the implementation to use str instead of the actual AutoSamplerSite

class AutoSamplerSite(BaseOntology):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLERSITE
    holds: Vial
    locationID: int

class AutoSampler(LabEquipment):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLER
    hasSite: List[AutoSamplerSite]

class VapourtecR4Reactor(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECR4REACTOR
    locationID: int
    hasReactorMaterial: Union[str, OntoCAPE_MaterialAmount] # NOTE TODO here str is provided as an optional to simplify the implementation
    hasInternalDiameter: OM_Diameter
    hasReactorLength: OM_Length
    hasReactorVolume: OM_Volume
    hasReactorTemperatureLowerLimit: OM_CelsiusTemperature
    hasReactorTemperatureUpperLimit: OM_CelsiusTemperature
    conducted: List[Union[str, ReactionExperiment]] = None # TODO here we provided str as an optional to simplify the implementation

    @pydantic.root_validator
    @classmethod
    def reactor_temperature_operation_limit(cls, values):
        if values.get('hasReactorTemperatureLowerLimit').hasValue.hasNumericalValue > values.get('hasReactorTemperatureUpperLimit').hasValue.hasNumericalValue:
            raise Exception(
                'VapourtecR4Reactor <%s> has a ReactorTemperatureLowerLimit (%s) that is higher than the ReactorTemperatureUpperLimit (%s).' % (
                    values.get('instance_iri'), values.get('hasReactorTemperatureLowerLimit').hasValue.hasNumericalValue, values.get('hasReactorTemperatureUpperLimit').hasValue.hasNumericalValue
                )
            )
        return values

class VapourtecR2Pump(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECR2PUMP
    locationID: int

class VapourtecRS400(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECRS400
