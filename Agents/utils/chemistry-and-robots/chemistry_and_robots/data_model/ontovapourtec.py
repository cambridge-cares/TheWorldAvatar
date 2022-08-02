from __future__ import annotations
import pydantic
from typing import List, Union

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import *
from chemistry_and_robots.data_model.ontolab import *

LIST_ONTOVAPOURTEC_VALIDSTATE = [ONTOVAPOURTEC_IDLE, ONTOVAPOURTEC_NULL, ONTOVAPOURTEC_FAULTY,
ONTOVAPOURTEC_INACTIVE, ONTOVAPOURTEC_CLEANINGREACTION, ONTOVAPOURTEC_REACTIONCOMPLETED,
ONTOVAPOURTEC_RUNNINGREACTION, ONTOVAPOURTEC_INITIALISING, ONTOVAPOURTEC_FINALCLEANING]

class SampleLoopVolumeSetting(VolumeSetting):
    clz: str = ONTOVAPOURTEC_SAMPLELOOPVOLUMESETTING

class StoichiometryRatioSetting(ParameterSetting):
    clz: str = ONTOVAPOURTEC_STOICHIOMETRYRATIOSETTING

class ReactorTemperatureSetting(TemperatureSetting):
    clz: str = ONTOVAPOURTEC_REACTORTEMPERATURESETTING

class ResidenceTimeSetting(DurationSetting):
    clz: str = ONTOVAPOURTEC_RESIDENCETIMESETTING

class ReactorSettings(EquipmentSettings):
    hasResidenceTimeSetting: ResidenceTimeSetting
    hasReactorTemperatureSetting: ReactorTemperatureSetting
    clz: str = ONTOVAPOURTEC_REACTORSETTING
    specifies: VapourtecR4Reactor

    def create_instance_for_kg(self, g: Graph, configure_digital_twin: bool) -> Graph:
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

        return super().create_instance_for_kg(g, configure_digital_twin)

class PumpSettings(EquipmentSettings):
    hasFlowRateSetting: Optional[FlowRateSetting] = None
    hasSampleLoopVolumeSetting: Optional[SampleLoopVolumeSetting] = None
    hasStoichiometryRatioSetting: Optional[StoichiometryRatioSetting]
    pumpsLiquidFrom: AutoSamplerSite
    clz: str = ONTOVAPOURTEC_PUMPSETTINGS
    specifies: VapourtecR2Pump

    def create_instance_for_kg(self, g: Graph, configure_digital_twin: bool) -> Graph:
        # TODO should we distinguish between hasFlowRateSetting, hasSampleLoopVolumeSetting, hasStoichiometryRatioSetting used in different occasions?
        # <pumpSetting> <OntoVapourtec:hasFlowRateSetting> <stoichisetting>
        if self.hasFlowRateSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASFLOWRATESETTING), URIRef(self.hasFlowRateSetting.instance_iri)))
            g = self.hasFlowRateSetting.create_instance_for_kg(g)

        # <pumpSetting> <OntoVapourtec:hasSampleLoopVolumeSetting> <stoichisetting>
        if self.hasSampleLoopVolumeSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASSAMPLELOOPVOLUMESETTING), URIRef(self.hasSampleLoopVolumeSetting.instance_iri)))
            g = self.hasSampleLoopVolumeSetting.create_instance_for_kg(g)

        # <pumpSetting> <OntoVapourtec:hasStoichiometryRatioSetting> <stoichisetting>
        if self.hasStoichiometryRatioSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASSTOICHIOMETRYRATIOSETTING), URIRef(self.hasStoichiometryRatioSetting.instance_iri)))
            g = self.hasStoichiometryRatioSetting.create_instance_for_kg(g)

        # <pumpSetting> <OntoVapourtec:pumpsLiquidFrom> <autosampler_site>
        g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_PUMPSLIQUIDFROM), URIRef(self.pumpsLiquidFrom.instance_iri)))

        return super().create_instance_for_kg(g, configure_digital_twin)

class Vial(BaseOntology):
    clz: str = ONTOVAPOURTEC_VIAL
    isFilledWith: Optional[ChemicalSolution] = None # NOTE ChemicalSolution is made optional to accommodate situation where vial is empty
    hasFillLevel: OM_Volume
    # hasWarningLevel: OM_Volume # NOTE hasWarningLevel is temporarily commented out before a decision is made whether keep it
    # TODO [when run in loop] bring hasWarningLevel back, this is needed when the fill level is below certain amount to remind the reseachers to add liquid
    hasMaxLevel: OM_Volume
    isHeldIn: str # NOTE here we simplify the implementation to use str instead of the actual AutoSamplerSite

class AutoSamplerSite(BaseOntology):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLERSITE
    holds: Vial
    locationID: str

class AutoSampler(LabEquipment):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLER
    hasSite: List[AutoSamplerSite]

class VapourtecR4Reactor(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECR4REACTOR
    locationID: str
    hasReactorMaterial: Union[str, OntoCAPE_MaterialAmount] # NOTE TODO here str is provided as an optional to simplify the implementation
    hasInternalDiameter: OM_Diameter
    hasReactorLength: OM_Length
    hasReactorVolume: OM_Volume
    hasReactorTemperatureLowerLimit: OM_CelsiusTemperature
    hasReactorTemperatureUpperLimit: OM_CelsiusTemperature

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
    locationID: str

class VapourtecState(Saref_State):
    clz: str = ONTOVAPOURTEC_NULL # NOTE the default is set as Null
    stateLastUpdatedAt: float

class VapourtecRS400(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECRS400
    hasState: VapourtecState

class VapourtecInputFile(BaseOntology):
    clz: str = ONTOVAPOURTEC_VAPOURTECINPUTFILE
    lastLocalModifiedAt: float
    lastUploadedAt: float
    localFilePath: str
    remoteFilePath: str

#########################################
## Put all update_forward_refs() below ##
#########################################
ReactorSettings.update_forward_refs()
PumpSettings.update_forward_refs()
