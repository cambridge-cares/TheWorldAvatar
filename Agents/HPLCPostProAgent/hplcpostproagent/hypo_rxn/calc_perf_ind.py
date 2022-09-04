from hplcpostproagent.hypo_rxn.hypo_rxn import *

import chemistry_and_robots.kg_operations.unit_conversion as unit_conv

TIME_TEMPERATURE_ECO_SCORE_FACTOR = 0.002
AMBIENT_TEMPERATURE_DEGREECELSIUS = 25
ECO_SCORE_BASE_VALUE = 100

def calculate_performance_indicator(
    hypo_reactor: HypoReactor,
    hypo_end_stream: HypoEndStream,
    rxn_exp_instance: ReactionExperiment,
    target_clz: str
) -> List[PerformanceIndicator]:
    """
        This method calculates the value of performance indicator and returns an instance of PerformanceIndicator.
        Arguments:
            hypo_reactor - instance of HypoReactor
            hypo_end_stream - instance of HypoEndStream
            rxn_exp_instance - instance of ontorxn.ReactionExperiment from chemistry_and_robots package
            target_clz - class iri of the target PerformanceIndicator
    """

    # Locate the reference instance of the PerformanceIndicator in the source ReactionExperiment that the ReactionVariation instance isVariationOf
    # For those PerformanceIndicator that NO reference instance are located, we compute directly
    lst_reference = locate_reference_performance_indicator(rxn_exp_instance, target_clz)
    # Create and return the list of PerformanceIndicator instances depends on target_clz
    if target_clz == ONTOREACTION_YIELD:
        return [calculate_yield(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    elif target_clz == ONTOREACTION_CONVERSION:
        return [calculate_conversion(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    elif target_clz == ONTOREACTION_ECOSCORE:
        return [calculate_eco_score(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    elif target_clz == ONTOREACTION_ENVIRONMENTALFACTOR:
        return [calculate_enviromental_factor(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    elif target_clz == ONTOREACTION_SPACETIMEYIELD:
        return [calculate_space_time_yield(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    elif target_clz == ONTOREACTION_RUNMATERIALCOST:
        return [calculate_run_material_cost(rxn_exp_instance, hypo_reactor, hypo_end_stream, pi) for pi in lst_reference]
    else:
        raise NotImplementedError("Requested target clz <%s> as PerformanceIndicator is NOT yet supported when post-processing ReactionExperiment: %s." % (
            target_clz, str(rxn_exp_instance)))

def calculate_yield(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction yield based on the non-catalyst yield limiting reactant (the one with the lowest run concentration)."""
    yield_limiting_species = retrieve_yield_limiting_species(hypo_reactor)
    target_product_species = retrieve_product_species(hypo_end_stream)

    yield_limiting_conc = unit_conv.unit_conversion_return_value_dq(yield_limiting_species.run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT)
    prod_run_conc = unit_conv.unit_conversion_return_value_dq(target_product_species.run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT)

    _yield = round(prod_run_conc / yield_limiting_conc, 4) # Round the decimal place

    pi_yield = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _yield, unit_conv.UNIFIED_YIELD_UNIT)
    pi_yield.yieldLimitingSpecies = yield_limiting_species.species_iri # Also set the yieldLimitingSpecies

    return pi_yield

def calculate_conversion(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction conversion based on the non-catalyst yield limiting reactant (the one with the lowest run concentration)."""
    # NOTE here the conversion is calculated based on the yield limiting species
    yield_limiting_species = retrieve_yield_limiting_species(hypo_reactor)
    _species_in_end_stream = [species for species in hypo_end_stream.component if species.species_iri == yield_limiting_species.species_iri][0]

    yield_limiting_conc = unit_conv.unit_conversion_return_value_dq(yield_limiting_species.run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT)
    unreacted_conc = unit_conv.unit_conversion_return_value_dq(_species_in_end_stream.run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT)

    _conversion = round(1 - unreacted_conc / yield_limiting_conc, 4) # Round the decimal place

    pi_conversion = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _conversion, unit_conv.UNIFIED_CONVERSION_UNIT)
    pi_conversion.yieldLimitingSpecies = yield_limiting_species.species_iri # Also set the yieldLimitingSpecies

    return pi_conversion

def calculate_space_time_yield(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction space time yield, which commonly has (kg per litre per minute) as its unit."""
    target_product_species = retrieve_product_species(hypo_end_stream)

    prod_run_mass = unit_conv.unit_conversion_return_value_dq(target_product_species._run_mass, unit_conv.UNIFIED_MASS_UNIT)
    residence_time = unit_conv.unit_conversion_return_value_dq(hypo_reactor.residence_time, unit_conv.UNIFIED_TIME_UNIT)
    reactor_volume = unit_conv.unit_conversion_return_value_dq(hypo_reactor.reactor_volume, unit_conv.UNIFIED_VOLUME_UNIT)

    _sty = round(prod_run_mass / residence_time / reactor_volume, 2) # Round the decimal place

    pi_sty = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _sty, unit_conv.UNIFIED_SPACETIMEYIELD_UNIT)

    return pi_sty

def calculate_eco_score(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction eco score."""
    residence_time = unit_conv.unit_conversion_return_value_dq(hypo_reactor.residence_time, unit_conv.UNIFIED_TIME_UNIT)
    reactor_temperature = unit_conv.unit_conversion_return_value_dq(hypo_reactor.reactor_temperature, OM_DEGREECELSIUS)
    time_temperature_eco_score = TIME_TEMPERATURE_ECO_SCORE_FACTOR * residence_time * (
        (reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS) * (reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS) / abs(reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS))
    total_run_eco_score = retrieve_total_run_eco_score(hypo_reactor)
    _eco_score = round(ECO_SCORE_BASE_VALUE - time_temperature_eco_score - total_run_eco_score, 2) # Round the decimal place

    pi_eco_score = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _eco_score, unit_conv.UNIFIED_ECOSCORE_UNIT)

    return pi_eco_score

def calculate_enviromental_factor(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction environmental factor."""
    target_product_species = retrieve_product_species(hypo_end_stream)
    all_reactant = [s for inlet in hypo_reactor.inlet_run_stream for s in inlet.solute if s._is_reactant]
    all_solvent = [inlet.solvent for inlet in hypo_reactor.inlet_run_stream]
    reactant_and_solvent = all_reactant + all_solvent

    total_reac_n_solvent_run_mass = sum([unit_conv.unit_conversion_return_value_dq(s._run_mass, unit_conv.UNIFIED_MASS_UNIT) for s in reactant_and_solvent])
    prod_run_mass = unit_conv.unit_conversion_return_value_dq(target_product_species._run_mass, unit_conv.UNIFIED_MASS_UNIT)

    _e_factor = round(prod_run_mass / (total_reac_n_solvent_run_mass - prod_run_mass), 2) # Round the decimal place

    pi_e_factor = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _e_factor, unit_conv.UNIFIED_ENVIRONMENTFACTOR_UNIT)

    return pi_e_factor

def calculate_run_material_cost(rxn_exp_instance: ReactionExperiment, hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, reference_performance_indicator: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction material cost for a single run, which commonly has (pound sterling) as its unit."""
    all_reactant = [s for inlet in hypo_reactor.inlet_run_stream for s in inlet.solute if s._is_reactant]
    all_solvent = [inlet.solvent for inlet in hypo_reactor.inlet_run_stream]
    reactant_and_solvent = all_reactant + all_solvent
    # NOTE here the unit of the _run_volume and def_cost should already be standardised at creation of each HypoStreamSpecies instance
    # NOTE therefore the unit conversion is omitted
    _run_material_cost = round(sum([s._run_volume.hasNumericalValue * s.def_cost.hasNumericalValue for s in reactant_and_solvent]), 2) # Round the decimal place

    pi_run_material_cost = create_performance_indicator_instance(rxn_exp_instance, reference_performance_indicator, _run_material_cost, unit_conv.UNIFIED_RUN_MATERIAL_COST_UNIT)

    return pi_run_material_cost

def locate_reference_performance_indicator(
    rxn_exp_instance: ReactionExperiment,
    target_clz: str
) -> PerformanceIndicator:
    """This method creates a placeholder PerformanceIndicator instance that to be added to the ReactionExperiment instance."""
    if isinstance(rxn_exp_instance, ReactionVariation):
        lst_target = [pi for pi in rxn_exp_instance.isVariationOf.hasPerformanceIndicator if pi.clz == target_clz]
        if len(lst_target) == 1:
            return lst_target
        elif len(lst_target) == 0:
            return [PerformanceIndicator(
                instance_iri="http://placeholder",
                clz=target_clz,
                rxn_exp_iri=rxn_exp_instance.instance_iri, # The value here should NOT matter as it should not be accessed by any codes
                objPropWithExp=OBJECT_RELATIONSHIP_PERFORMANCE_INDICATOR_RXN_EXP_DICT[target_clz],
                hasValue=None,
                positionalID=None
            )]
        else:
            raise Exception("Multiple target PerformanceIndicator with a clz <%s> is NOT yet supported, identified in ReactionExperiment: %s" % (
                target_clz, str(rxn_exp_instance)))
    else:
        raise NotImplementedError("Post processing ReactionExperiment is NOT yet supported.")

def create_performance_indicator_instance(
    rxn_exp_instance: ReactionExperiment,
    reference_performance_indicator: PerformanceIndicator,
    numerical_value,
    unit: str
) -> PerformanceIndicator:
    """This method creates the instance of PerformanceIndicator given the placeholder instance and the computed value to be used for OM_Measure."""
    _objPropWithExp = reference_performance_indicator.objPropWithExp
    if ONTOREACTION_HASPERFORMANCEINDICATOR not in reference_performance_indicator.objPropWithExp:
        _objPropWithExp.append(ONTOREACTION_HASPERFORMANCEINDICATOR)

    performance_indicator_instance = PerformanceIndicator(
        instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
        namespace_for_init=getNameSpace(rxn_exp_instance.instance_iri),
        clz=reference_performance_indicator.clz,
        rxn_exp_iri=rxn_exp_instance.instance_iri,
        objPropWithExp=_objPropWithExp,
        hasValue=OM_Measure(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(rxn_exp_instance.instance_iri),
            hasUnit=unit,
            hasNumericalValue=numerical_value
        ),
        positionalID=reference_performance_indicator.positionalID
    )
    return performance_indicator_instance

def retrieve_yield_limiting_species(hypo_reactor: HypoReactor) -> HypoStreamSpecies:
    """This method retrieves the yield limiting reactant given the instance of HypoReactor."""
    all_inlet_stream = hypo_reactor.inlet_run_stream
    all_reactant_species = {r.species_iri:r for reac in all_inlet_stream for r in reac.solute if r._is_reactant}
    all_reactant_conc = {all_reactant_species[s].species_iri:all_reactant_species[s].run_conc for s in all_reactant_species}
    yield_limiting_conc = min([unit_conv.unit_conversion_return_value_dq(all_reactant_conc[dq], unit_conv.UNIFIED_CONCENTRATION_UNIT) for dq in all_reactant_conc])
    yield_limiting_species_lst = [species for species in all_reactant_conc if all_reactant_conc[species].hasNumericalValue == yield_limiting_conc]
    if len(yield_limiting_species_lst) > 1:
        raise NotImplementedError("Processing multiple yield limiting species in the reactor input streams is NOT yet supported, the HypoReactor: %s" % (
            str(hypo_reactor)))
    else:
        yield_limiting_species_iri = yield_limiting_species_lst[0]
    return all_reactant_species[yield_limiting_species_iri]

def retrieve_product_species(hypo_end_stream: HypoEndStream) -> HypoStreamSpecies:
    """This method retrieves the product species given the instance of HypoEndStream."""
    all_target_product = [comp for comp in hypo_end_stream.component if comp._is_target_product]

    if len(all_target_product) > 1:
        raise NotImplementedError("Multiple TargetProduct in the end stream is NOT yet supported: %s" % (str(all_target_product)))
    elif len(all_target_product) < 1:
        raise Exception("No TargetProduct identified in the end stream %s" % (str(hypo_end_stream)))
    else:
        target_product = all_target_product[0]
    return target_product

def retrieve_total_run_eco_score(hypo_reactor: HypoReactor):
    """This method retrieves tht total run eco score given the instance of HypoReactor."""
    all_solute = [s for inlet in hypo_reactor.inlet_run_stream for s in inlet.solute]
    all_solvent = [inlet.solvent for inlet in hypo_reactor.inlet_run_stream]
    all_species = all_solute + all_solvent
    total_run_eco_score = sum([unit_conv.unit_conversion_return_value_dq(s.def_eco_score, unit_conv.UNIFIED_ECOSCORE_UNIT) for s in all_species])
    return total_run_eco_score
