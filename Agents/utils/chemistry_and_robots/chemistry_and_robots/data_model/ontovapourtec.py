from __future__ import annotations
import pydantic
from typing import List, Union, Tuple

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import *
from chemistry_and_robots.data_model.ontolab import *
import chemistry_and_robots.data_model.unit_conversion as unit_conv

LIST_ONTOVAPOURTEC_VALIDSTATE = [ONTOVAPOURTEC_IDLE, ONTOVAPOURTEC_NULL, ONTOVAPOURTEC_FAULTY,
ONTOVAPOURTEC_INACTIVE, ONTOVAPOURTEC_CLEANINGREACTION, ONTOVAPOURTEC_REACTIONCOMPLETED,
ONTOVAPOURTEC_RUNNINGREACTION, ONTOVAPOURTEC_INITIALISING, ONTOVAPOURTEC_FINALCLEANING, ONTOVAPOURTEC_DRYRUNSTATE]

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
    # TODO [future work] pumpsLiquidFrom should be pointing to the super class of Vial or ReagentBottle (the container that actually holds the liquid)
    pumpsLiquidFrom: Optional[AutoSamplerSite]
    clz: str = ONTOVAPOURTEC_PUMPSETTINGS
    specifies: VapourtecR2Pump

    @pydantic.root_validator
    @classmethod
    def pump_setting_have_at_least_one_source(cls, values):
        if values.get('pumpsLiquidFrom') is None and values.get('specifies').hasReagentSource is None:
            raise Exception(
                f"Pump <{values.get('specifies').instance_iri}> is not connected to any ReagentBottle. "
                f"Therefore, it must source liquid from an AutoSamplerSite that to be pointed by pumpsLiquidFrom. "
                f"Please consifer to add pumpsLiquidFrom to the PumpSettings, or connect the pump to a ReagentBottle."
            )
        return values

    def create_instance_for_kg(self, g: Graph, configure_digital_twin: bool) -> Graph:
        # TODO [future work] should we distinguish between hasFlowRateSetting, hasSampleLoopVolumeSetting, hasStoichiometryRatioSetting used in different occasions?
        # <pumpSetting> <OntoVapourtec:hasFlowRateSetting> <stoichisetting>
        if self.hasFlowRateSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASFLOWRATESETTING), URIRef(self.hasFlowRateSetting.instance_iri)))
            g = self.hasFlowRateSetting.create_instance_for_kg(g)
            # TODO [future work] should find a generic way of creating instance for parameter setting
            # NOTE add quantity of flowrate to KG, this is a hack as FlowRate is not part of initial ReactionCondition
            flowrate_quantity = self.hasFlowRateSetting.hasQuantity
            # <flowrate_quantity_iri> <rdf:type> <flowrate_quantity_clz> .
            g.add((URIRef(flowrate_quantity.instance_iri), RDF.type, URIRef(flowrate_quantity.clz)))
            # Add below triples following units of measure practices:
            # <flowrate_quantity_iri> <om:hasValue> <flowrate_quantity_measure_iri> .
            g.add((URIRef(flowrate_quantity.instance_iri), URIRef(OM_HASVALUE), URIRef(flowrate_quantity.hasValue.instance_iri)))
            # Add triples for units of measure
            g = flowrate_quantity.hasValue.create_instance_for_kg(g)

        # <pumpSetting> <OntoVapourtec:hasSampleLoopVolumeSetting> <stoichisetting>
        if self.hasSampleLoopVolumeSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASSAMPLELOOPVOLUMESETTING), URIRef(self.hasSampleLoopVolumeSetting.instance_iri)))
            g = self.hasSampleLoopVolumeSetting.create_instance_for_kg(g)

        # <pumpSetting> <OntoVapourtec:hasStoichiometryRatioSetting> <stoichisetting>
        if self.hasStoichiometryRatioSetting is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_HASSTOICHIOMETRYRATIOSETTING), URIRef(self.hasStoichiometryRatioSetting.instance_iri)))
            g = self.hasStoichiometryRatioSetting.create_instance_for_kg(g)

        # only add below triples if pumpsLiquidFrom is not None, i.e. it pumps from autosampler site
        # <pumpSetting> <OntoVapourtec:pumpsLiquidFrom> <autosampler_site>
        if self.pumpsLiquidFrom is not None:
            g.add((URIRef(self.instance_iri), URIRef(ONTOVAPOURTEC_PUMPSLIQUIDFROM), URIRef(self.pumpsLiquidFrom.instance_iri)))

        return super().create_instance_for_kg(g, configure_digital_twin)

class Vial(ChemicalContainer):
    clz: str = ONTOLAB_VIAL

class AutoSamplerSite(BaseOntology):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLERSITE
    holds: Vial
    locationID: str

    def get_chemical_given_chemical_species(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
        species_to_exclude: List[str]=None,
        only_allow_fully_identified_material: bool=True,
    ) -> Optional[Chemical]:
        if self.holds is not None:
            if self.holds.isFilledWith is not None:
                if self.holds.isFilledWith.refersToMaterial is not None:
                    # below first check if only source this material if it's fully identified, and whether it's fully identified
                    # this offers flexibility if we want the intermediate material in multi-step reaction to be included
                    if only_allow_fully_identified_material and self.holds.isFilledWith.containsUnidentifiedComponent:
                        # if only_allow_fully_identified_material is True, and the material is not fully identified, return None
                        return None
                    else:
                        # for all other cases, check if the material is the one we want
                        if self.holds.isFilledWith.refersToMaterial.contains_chemical_species(solute, solvent_as_constraint, species_to_exclude):
                            return self.holds.isFilledWith.refersToMaterial
        return None

class AutoSampler(LabEquipment):
    clz: str = ONTOVAPOURTEC_AUTOSAMPLER
    hasSite: List[AutoSamplerSite]
    sampleLoopVolume: OM_Volume

    def is_able_to_provide_required_chemicals(self, required_chemicals: List[InputChemical]) -> bool:
        if len(required_chemicals) == 0: return True
        return all(item in [site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour for site in self.hasSite if site.holds.isFilledWith is not None] for item in [input_chem.thermodynamicBehaviour for input_chem in required_chemicals])

    def get_autosampler_site_given_input_chemical(self, input_chem: InputChemical) -> Optional[AutoSamplerSite]:
        list_possible_site = [site for site in self.hasSite if site.holds.isFilledWith is not None and site.holds.isFilledWith.refersToMaterial is not None and site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour == input_chem.thermodynamicBehaviour]
        list_site_enough_chemical = []
        # check if the chemical in the site is enough for the reaction
        # TODO [future work] for now it relies on the warning level to be greater than the required amount for one reaction
        #     in the future, the fill level should be checked against the actual required amount for one reaction dynamically
        for site in list_possible_site:
            fill_level_dq = unit_conv.DimensionalQuantity(
                hasUnit=site.holds.hasFillLevel.hasValue.hasUnit,
                hasNumericalValue=site.holds.hasFillLevel.hasValue.hasNumericalValue,
            )
            fill_level_dq_in_ml = fill_level_dq.convert_to(OM_MILLILITRE)
            warn_level_dq = unit_conv.DimensionalQuantity(
                hasUnit=site.holds.hasWarningLevel.hasValue.hasUnit,
                hasNumericalValue=site.holds.hasWarningLevel.hasValue.hasNumericalValue,
            )
            warn_level_dq_in_ml = warn_level_dq.convert_to(OM_MILLILITRE)
            if fill_level_dq_in_ml.hasNumericalValue >= warn_level_dq_in_ml.hasNumericalValue:
                list_site_enough_chemical.append(site)

        return list_site_enough_chemical[0] if len(list_site_enough_chemical) > 0 else None

class VapourtecR4Reactor(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECR4REACTOR
    locationID: str
    hasReactorMaterial: Union[str, OntoCAPE_MaterialAmount] # NOTE TODO [future work] here str is provided as an optional to simplify the implementation
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

    def is_suitable_for_reaction_experiment(self, rxn_exp: ReactionExperiment) -> bool:
        # TODO [in next iteration] add check for pressure etc.

        # Check if the reaction temperature is within the operation limit
        reaction_temperature = rxn_exp.get_reaction_condition(ONTOREACTION_REACTIONTEMPERATURE, None)
        # TODO [in next iteration] handle different temperature units
        if self.hasReactorTemperatureLowerLimit.hasValue.hasUnit != reaction_temperature.hasValue.hasUnit:
            raise Exception(
                f'VapourtecR4Reactor <{self.instance_iri}> has a ReactorTemperatureLowerLimit unit ({self.hasReactorTemperatureLowerLimit.hasValue.hasUnit}) that is not compatible with the reaction temperature unit ({reaction_temperature.hasValue.hasUnit}).'
            )
        if self.hasReactorTemperatureUpperLimit.hasValue.hasUnit != reaction_temperature.hasValue.hasUnit:
            raise Exception(
                f'VapourtecR4Reactor <{self.instance_iri}> has a ReactorTemperatureUpperLimit unit ({self.hasReactorTemperatureUpperLimit.hasValue.hasUnit}) that is not compatible with the reaction temperature unit ({reaction_temperature.hasValue.hasUnit}).'
            )
        return self.hasReactorTemperatureLowerLimit.hasValue.hasNumericalValue <= reaction_temperature.hasValue.hasNumericalValue <= self.hasReactorTemperatureUpperLimit.hasValue.hasNumericalValue

class VapourtecR2Pump(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECR2PUMP
    locationID: str
    hasReagentSource: Optional[ReagentBottle]

    def is_reference_pump(self) -> bool:
        return self.locationID == "A"

    def get_reagent_chemical_amount(self) -> Optional[ChemicalAmount]:
        return self.hasReagentSource.isFilledWith if self.hasReagentSource is not None else None

class VapourtecState(Saref_State):
    clz: str = ONTOVAPOURTEC_NULL # NOTE the default is set as Null
    stateLastUpdatedAt: float

class CollectionMethod(BaseOntology):
    clz: str = ONTOVAPOURTEC_COLLECTIONMETHOD
    toReceptacle: Optional[str] # NOTE this is made optional to accommodate ONTOVAPOURTEC_FRACTIONCOLLECTOR
    # when instantiation, the clz can be set to one of the following:
    # ONTOVAPOURTEC_SINGLERECEPTACLE, ONTOVAPOURTEC_FRACTIONCOLLECTOR

class VapourtecRS400(LabEquipment):
    clz: str = ONTOVAPOURTEC_VAPOURTECRS400
    hasState: VapourtecState
    hasCollectionMethod: CollectionMethod
    recommendedReactionScale: Optional[OM_Volume]

    # def get_collection_site_number(self, collection_site: str) -> int:
    #     return int([site.locationID for site in self.get_autosampler().hasSite if site.instance_iri == collection_site][0])

    # def get_collection_site_vial_iri(self, collection_site: str) -> str:
    #     return [site.holds.instance_iri for site in self.get_autosampler().hasSite if site.instance_iri == collection_site][0]

    def get_collection_site(self, collection_volume_in_ml: float=0) -> Tuple[Optional[str], Optional[str], Optional[str]]:
        collection_site = [site for site in self.get_autosampler().hasSite if site.holds is not None and site.holds.isFilledWith is None and site.holds.hasMaxLevel.hasValue.hasNumericalValue > collection_volume_in_ml * 1.2]
        if len(collection_site) == 0: return None, None, None
        if len(collection_site) > 0: return collection_site[0].instance_iri, int(collection_site[0].locationID), collection_site[0].holds.instance_iri

    def collects_to_single_receptacle(self) -> bool:
        return self.hasCollectionMethod.clz == ONTOVAPOURTEC_SINGLERECEPTACLE

    def collects_to_fraction_collector(self) -> bool:
        return self.hasCollectionMethod.clz == ONTOVAPOURTEC_FRACTIONCOLLECTOR

    def get_collection_receptacle(self):
        return self.hasCollectionMethod.toReceptacle if self.collects_to_single_receptacle() else None

    def has_access_to_chemical_species(
        self,
        solute: str,
        solvent_as_constraint: List[str]=None,
        species_to_exclude: List[str]=None,
    ) -> Chemical:
        # first check if any of the reagent bottles contains the chemical species
        reagent_bottle = self.get_reagent_bottle_of_r2_pumps()
        if bool(reagent_bottle):
            lst_material = [
                reagent_bottle.isFilledWith.refersToMaterial for reagent_bottle in reagent_bottle if reagent_bottle.contains_chemical_species(
                    solute=solute,
                    solvent_as_constraint=solvent_as_constraint,
                    species_to_exclude=species_to_exclude,
                )
            ]
            if bool(lst_material):
                return lst_material[0]

        # then only if there is pump sourcing from autosampler
        # check if the chemical species can be provided by the autosampler
        if bool(self.get_r2_pump_source_from_autosampler()):
            # if yes, can return now
            lst_material = [
                site.get_chemical_given_chemical_species(
                    solute=solute,
                    solvent_as_constraint=solvent_as_constraint,
                    species_to_exclude=species_to_exclude,
                ) for site in self.get_autosampler().hasSite if site.get_chemical_given_chemical_species(
                    solute=solute,
                    solvent_as_constraint=solvent_as_constraint,
                    species_to_exclude=species_to_exclude,
                ) is not None]
            if bool(lst_material):
                return lst_material[0]

        return None

    def is_suitable_for_reaction_experiment(self, rxn_exp: ReactionExperiment) -> bool:
        # first check the reactor, if no reactor is suitable, then return False
        if self.get_suitable_r4_reactor_for_reaction_experiment(rxn_exp) is None:
            return False

        # second filter the reagent bottles and autosampler based on the input chemicals, i.e. is the reactor able to get all the required input chemicals
        # get the list of reagent bottles
        list_reagent_bottle_single_phase = [pump.hasReagentSource.isFilledWith.refersToMaterial.thermodynamicBehaviour for pump in self.get_r2_pumps() if pump.hasReagentSource is not None]
        # get the list of input chemicals that are not provided in the list of reagent bottles
        list_input_chemical_required_from_autosampler = [input_chem for input_chem in rxn_exp.hasInputChemical if input_chem.thermodynamicBehaviour not in list_reagent_bottle_single_phase]
        # check if the amount of pumps that can pump the input chemicals from the autosampler is enough, i.e. the pumps that are not connected to reagent bottles
        if len(list_input_chemical_required_from_autosampler) > len(self.get_r2_pumps()) - len(list_reagent_bottle_single_phase):
            return False
        # get the autosampler and check if it can provide all the required input chemicals
        if len(list_input_chemical_required_from_autosampler) > 0:
            if not self.get_autosampler().is_able_to_provide_required_chemicals(list_input_chemical_required_from_autosampler):
                return False
        # also check that the vapourtec rs400 module is idle
        if self.hasState.clz != ONTOVAPOURTEC_IDLE:
            return False
        if self.isManagedBy is None:
            # TODO [nice-to-have] here we can add functions to inform the owner of hardware to spin up agent for execution
            return False

        return True

    def get_r4_reactors(self) -> List[VapourtecR4Reactor]:
        return [reactor for reactor in self.consistsOf if isinstance(reactor, VapourtecR4Reactor)]

    def get_r4_reactor(self, reactor_iri: str) -> VapourtecR4Reactor:
        return [reactor for reactor in self.get_r4_reactors() if reactor.instance_iri == reactor_iri][0]

    def get_suitable_r4_reactor_for_reaction_experiment(self, rxn_exp: ReactionExperiment) -> Optional[VapourtecR4Reactor]:
        list_suitable_reactor = [reactor for reactor in self.get_r4_reactors() if reactor.is_suitable_for_reaction_experiment(rxn_exp)]
        if len(list_suitable_reactor) == 0:
            return None
        return list_suitable_reactor[0]

    def get_r2_pumps(self) -> List[VapourtecR2Pump]:
        return [pump for pump in self.consistsOf if isinstance(pump, VapourtecR2Pump)]

    def get_reference_r2_pump(self) -> VapourtecR2Pump:
        return [pump for pump in self.get_r2_pumps() if pump.is_reference_pump()][0]

    def get_r2_pump_source_from_bottle(self) -> Optional[List[VapourtecR2Pump]]:
        return [pump for pump in self.get_r2_pumps() if pump.hasReagentSource is not None]

    def get_r2_pump_source_from_autosampler(self) -> Optional[List[VapourtecR2Pump]]:
        # any pump that not sourcing from a reagent bottle is considered as sourcing from the autosampler
        return [pump for pump in self.get_r2_pumps() if pump.hasReagentSource is None]

    def get_reagent_bottle_of_r2_pumps(self):
        return [pump.hasReagentSource for pump in self.get_r2_pumps() if pump.hasReagentSource is not None]

    def locate_r2_pump_given_input_chemical(
        self,
        input_chemical: InputChemical,
        assigned_pumps: List[str]=None,
        exclude_pumps: List[str]=None,
        is_reactant: bool=True,
    ) -> Optional[VapourtecR2Pump]:
        # this function is used to locate the pump that can pump the input chemical
        # first it checks if the input chemical is in the reagent bottles, if yes, then return the pump that is connected to the reagent bottle
        # second, it returns the first pump that is not connected to the reagent bottle
        # NOTE that in the second case the returned pump is simply available for pumping liquid from the autosampler
        # it does not mean that the autosampler has the required input chemical
        # TODO [future work] NOTE it is assumed that the input chemical contains reactant will be pumped from the first a few pumps
        #     i.e. ideally, the reference pump pump the main reactant, the second pump pump the second reactant, etc. and the last pump pump the catalyst
        #     however, this in theory depends on the physical connection and how the pipes are connected
        #     in future work, this information should be provided in the knowledge graph and queried dynamically
        assigned_pumps = assigned_pumps if assigned_pumps is not None else []
        exclude_pumps = exclude_pumps if exclude_pumps is not None else []
        _not_to_select_pumps = assigned_pumps + exclude_pumps
        for pump in self.get_r2_pump_source_from_bottle():
            if pump.hasReagentSource.isFilledWith.refersToMaterial.thermodynamicBehaviour == input_chemical.thermodynamicBehaviour and pump.instance_iri not in _not_to_select_pumps:
                return pump
        lst_pumps_from_autosampler = {pump.locationID:pump for pump in self.get_r2_pump_source_from_autosampler() if pump.instance_iri not in _not_to_select_pumps}
        # Return None if no pump is available
        if len(lst_pumps_from_autosampler) == 0: return None
        # Sort the pumps by location ID and return the first or last pump depends on if the input chemical is reactant or not
        _sorted_lst = {key:lst_pumps_from_autosampler[key] for key in sorted(lst_pumps_from_autosampler.keys())}
        if is_reactant:
            return list(_sorted_lst.values())[0]
        else:
            return list(_sorted_lst.values())[-1]

    def get_autosampler(self) -> AutoSampler:
        # NOTE this method assumes there's only one AutoSampler associated with one VapourtecRS400 module
        for equip in self.consistsOf:
            if isinstance(equip, AutoSampler):
                return equip
        return None

    def create_equip_settings_for_rs400_from_rxn_exp(self, rxn_exp: ReactionExperiment) -> List[EquipmentSettings]:
        # Preparation
        # 1. Get the list of reactant species
        if rxn_exp.isOccurenceOf is None:
            raise NotImplementedError(f"The constructed ReactionExperiment <{rxn_exp.instance_iri}> instance is not associated with any ChemicalReaction, please modify the query to include the reaction")
        reactant_species_iris = rxn_exp.isOccurenceOf.get_list_of_reactant()
        # 2. Create an empty list for equipment settings
        list_equip_setting = []
        # 3. Get the residence time condition
        res_time_cond = rxn_exp.get_reaction_condition(ONTOREACTION_RESIDENCETIME)
        # 4. Check if the reaction experiment is assigned to any reactor and get the preferred reactor
        if rxn_exp.isAssignedTo is None:
            raise Exception(f"The reaction experiment <{rxn_exp.instance_iri}> is not assigned to any reactor yet")
        preferred_r4_reactor = self.get_r4_reactor(rxn_exp.isAssignedTo)

        # Start creating the equipment settings
        # I. Construct the reactor settings
        reactor_setting = ReactorSettings(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            hasResidenceTimeSetting=ResidenceTimeSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=res_time_cond,
                namespace_for_init=getNameSpace(rxn_exp.instance_iri)
            ),
            hasReactorTemperatureSetting=ReactorTemperatureSetting(
                instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                hasQuantity=rxn_exp.get_reaction_condition(ONTOREACTION_REACTIONTEMPERATURE),
                namespace_for_init=getNameSpace(rxn_exp.instance_iri)
            ),
            namespace_for_init=getNameSpace(rxn_exp.instance_iri),
            specifies=preferred_r4_reactor,
            wasGeneratedFor=rxn_exp.instance_iri
        )
        list_equip_setting.append(reactor_setting)

        # II. Construct the pump settings
        # get the dict for stoi ratio
        dict_stoi_ratio = {cond.indicatesMultiplicityOf:cond for cond in rxn_exp.hasReactionCondition if cond.clz == ONTOREACTION_STOICHIOMETRYRATIO}
        # identify the reference reactant - the reference chemical should be the one with the ReactionScale
        reaction_scale = rxn_exp.get_reaction_scale()

        reference_pump = self.get_reference_r2_pump()
        autosampler = self.get_autosampler()

        # calculate flowrate for all InputChemical
        res_time_dq = unit_conv.DimensionalQuantity(
            hasUnit=res_time_cond.hasValue.hasUnit,
            hasNumericalValue=res_time_cond.hasValue.hasNumericalValue
        )
        res_time_dq_in_min = res_time_dq.convert_to(OM_MINUTETIME)
        reactor_volume_dq = unit_conv.DimensionalQuantity(
            hasUnit=preferred_r4_reactor.hasReactorVolume.hasValue.hasUnit,
            hasNumericalValue=preferred_r4_reactor.hasReactorVolume.hasValue.hasNumericalValue
        )
        reactor_volume_dq_in_ml = reactor_volume_dq.convert_to(OM_MILLILITRE)
        dct_vol_flowrate = rxn_exp.calculate_volumetric_flow_rate_for_input_chemical(
            total_flowrate_unit=OM_MILLILITREPERMINUTETIME,
            # total_flowrate_value = reactor volume / residence time
            total_flowrate_value=reactor_volume_dq_in_ml.hasNumericalValue / res_time_dq_in_min.hasNumericalValue,
            all_solute=rxn_exp.isOccurenceOf.get_list_of_reactant_and_catalyst(),
        )

        assigned_pumps = []
        assignment = {} # key: input chemical iri, value: pump locationID
        for input_chem in rxn_exp.hasInputChemical:
            autosampler_site = None # initialise autosampler_site as None

            # note that if the ReactionScale is specified, then we also need to set the sample volume for the pump
            if input_chem.instance_iri == reaction_scale.indicatesUsageOf:
                # if this is the reference reactant then it also MUST be the reference pump
                ref_pump_chem_amount = reference_pump.get_reagent_chemical_amount()
                if ref_pump_chem_amount is not None:
                    # this means the reference_pump is pumping liquid from ReagentBottle
                    # therefore, the chemical in this pump must be the same as the reference chemical
                    if ref_pump_chem_amount.refersToMaterial.thermodynamicBehaviour != input_chem.thermodynamicBehaviour:
                        raise Exception(
                            f"""The reference pump <{reference_pump.instance_iri}> is pumping liquid from ReagentBottle <{reference_pump.hasReagentSource.instance_iri}>.
                            However, the chemical in this pump is not the same as the reference InputChemical in ReactionEexperiment <{rxn_exp.instance_iri}>:
                            {input_chem.dict()}."""
                        )
                else:
                    # this means the reference_pump is connected to the autosampler
                    # therefore, we need to locate the autosampler site that contains the reference chemical
                    autosampler_site = autosampler.get_autosampler_site_given_input_chemical(input_chem)
                    # also need to check the reaction scale is not greater than the sampleLoopVolume of the autosampler
                    converted_autosampler_sampleloopvolume = unit_conv.unit_conversion_return_value(
                        autosampler.sampleLoopVolume.hasValue.hasNumericalValue,
                        autosampler.sampleLoopVolume.hasValue.hasUnit,
                        reaction_scale.hasValue.hasUnit
                    )
                    if reaction_scale.hasValue.hasNumericalValue > converted_autosampler_sampleloopvolume:
                        raise Exception(
                            f"""The reaction scale <{reaction_scale.instance_iri}> ({reaction_scale.hasValue.hasNumericalValue} {reaction_scale.hasValue.hasUnit}) """
                            f"""exceeds the sampleLoopVolume of the autosampler <{autosampler.instance_iri}> ({converted_autosampler_sampleloopvolume} {reaction_scale.hasValue.hasUnit}). """
                            f"""This is not allowed. You might want to reduce the reaction scale or change the reference pump to source reagent from reagent bottle."""
                        )

                pump_setting = PumpSettings(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                    specifies=reference_pump,
                    hasStoichiometryRatioSetting=StoichiometryRatioSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                        hasQuantity=dict_stoi_ratio.get(input_chem.instance_iri)
                    ),
                    hasSampleLoopVolumeSetting=SampleLoopVolumeSetting( # this only for reference pump
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                        hasQuantity=reaction_scale
                    ),
                    hasFlowRateSetting=FlowRateSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                        hasQuantity=dct_vol_flowrate[input_chem.instance_iri]
                    ),
                    pumpsLiquidFrom=autosampler_site, # this will be None if the reference_pump is pumping liquid from ReagentBottle
                    wasGeneratedFor=rxn_exp.instance_iri
                )
                assigned_pumps.append(reference_pump.instance_iri)
                assignment[input_chem.instance_iri] = pump_setting.specifies.locationID
            else:
                # this means the input_chem is not the reference chemical
                # locate the pump that can be used to provide the chemical
                _pump = self.locate_r2_pump_given_input_chemical(
                    input_chem,
                    assigned_pumps,
                    exclude_pumps=[reference_pump.instance_iri],
                    is_reactant=input_chem.is_reactant_stream(reactant_species_iris=reactant_species_iris)
                )
                if _pump is None:
                    raise Exception(f"""No available pump found for InputChemical <{input_chem.instance_iri}> of ReactionExperiment <{rxn_exp.instance_iri}>,
                                    allocated pumps: { assignment }. Details about InputChemical: {input_chem.dict()}""")
                if _pump.instance_iri == reference_pump.instance_iri:
                    raise Exception(f"Reference pump <{reference_pump.instance_iri}> cannot be used to pump non-reference InputChemical <{input_chem.instance_iri}>.")
                if _pump.hasReagentSource is None:
                    # this means the pump is connected to the autosampler and we need to source InputChemical from autosampler
                    autosampler_site = autosampler.get_autosampler_site_given_input_chemical(input_chem)

                    if autosampler_site is None:
                        raise Exception(f"No autosampler site found for InputChemical of ReactionExperiment <{rxn_exp.instance_iri}>: {input_chem.dict()}")

                pump_setting = PumpSettings(
                    instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                    specifies=_pump,
                    hasStoichiometryRatioSetting=StoichiometryRatioSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                        hasQuantity=dict_stoi_ratio.get(input_chem.instance_iri)
                    ),
                    hasFlowRateSetting=FlowRateSetting(
                        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=getNameSpace(rxn_exp.instance_iri),
                        hasQuantity=dct_vol_flowrate[input_chem.instance_iri]
                    ),
                    pumpsLiquidFrom=autosampler_site, # this will be None if the reference_pump is pumping liquid from ReagentBottle
                    wasGeneratedFor=rxn_exp.instance_iri
                )
                assigned_pumps.append(_pump.instance_iri)
                assignment[input_chem.instance_iri] = pump_setting.specifies.locationID

            list_equip_setting.append(pump_setting)

        # TODO [future work, shortcut for now] III. Construct the BPR settings for pressure

        return list_equip_setting

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
