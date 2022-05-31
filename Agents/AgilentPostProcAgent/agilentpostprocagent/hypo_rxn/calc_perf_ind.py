from agilentpostprocagent.hypo_rxn.hypo_rxn import *

TIME_TEMPERATURE_ECO_SCORE_FACTOR = 0.002
AMBIENT_TEMPERATURE_DEGREECELSIUS = 25
ECO_SCORE_BASE_VALUE = 100

def calculate_performance_indicator(
    hypo_reactor: HypoReactor,
    hypo_end_stream: HypoEndStream,
    rxn_exp_instance: ReactionExperiment,
    target_clz: str,
    expected_amount: int
) -> List[PerformanceIndicator]:
    """
        This method calculates the value of performance indicator and returns an instance of PerformanceIndicator.
        Arguments:
            hypo_reactor - instance of HypoReactor
            hypo_end_stream - instance of HypoEndStream
            rxn_exp_instance - instance of ontorxn.ReactionExperiment from chemistry_and_robots package
            target_clz - class iri of the target PerformanceIndicator
            expected_amount - expected amount of appearances of the target class in the PerformanceIndicator of ontorxn.ReactionExperiment
    """

    # Locate the placeholder instance of the PerformanceIndicator in the given ReactionExperiment instance
    # NOTE that the length of the list should match the amount of expected_amount, an exception will be throw if not meet the condition
    lst_placeholder = locate_placeholder_performance_indicator(rxn_exp_instance, target_clz, expected_amount)
    # Create and return the list of PerformanceIndicator instances depends on target_clz
    if target_clz == ONTOREACTION_YIELD:
        return [calculate_yield(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    elif target_clz == ONTOREACTION_CONVERSION:
        return [calculate_conversion(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    elif target_clz == ONTOREACTION_ECOSCORE:
        return [calculate_eco_score(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    elif target_clz == ONTOREACTION_ENVIRONMENTALFACTOR:
        return [calculate_enviromental_factor(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    elif target_clz == ONTOREACTION_SPACETIMEYIELD:
        return [calculate_space_time_yield(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    elif target_clz == ONTOREACTION_RUNMATERIALCOST:
        return [calculate_run_material_cost(hypo_reactor, hypo_end_stream, pi) for pi in lst_placeholder]
    else:
        raise NotImplementedError("Requested target clz <%s> as PerformanceIndicator is NOT yet supported when post-processing ReactionExperiment: %s." % (
            target_clz, str(rxn_exp_instance)))

def calculate_yield(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction yield based on the non-catalyst yield limiting reactant (the one with the lowest run concentration)."""
    yield_limiting_species = retrieve_yield_limiting_species(hypo_reactor)
    target_product_species = retrieve_product_species(hypo_end_stream)

    yield_limiting_conc = utils.unit_conversion_return_value_dq(yield_limiting_species.run_conc, utils.UNIFIED_CONCENTRATION_UNIT)
    prod_run_conc = utils.unit_conversion_return_value_dq(target_product_species.run_conc, utils.UNIFIED_CONCENTRATION_UNIT)

    _yield = prod_run_conc / yield_limiting_conc

    pi_yield = create_performance_indicator_instance(placeholder_perf_ind, _yield, OM_ONE)

    return pi_yield

def calculate_conversion(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction conversion based on the non-catalyst yield limiting reactant (the one with the lowest run concentration)."""
    # NOTE here the conversion is calculated based on the yield limiting species
    # TODO further improvement maybe provided as to calculate conversion for all reactant, or specific reactant that upon request
    yield_limiting_species = retrieve_yield_limiting_species(hypo_reactor)
    _species_in_end_stream = [species for species in hypo_end_stream.component if species.species_iri == yield_limiting_species.species_iri][0]

    yield_limiting_conc = utils.unit_conversion_return_value_dq(yield_limiting_species.run_conc, utils.UNIFIED_CONCENTRATION_UNIT)
    unreacted_conc = utils.unit_conversion_return_value_dq(_species_in_end_stream.run_conc, utils.UNIFIED_CONCENTRATION_UNIT)

    _conversion = 1 - unreacted_conc / yield_limiting_conc

    pi_conversion = create_performance_indicator_instance(placeholder_perf_ind, _conversion, OM_ONE)

    return pi_conversion

def calculate_space_time_yield(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction space time yield, which commonly has (kg per litre per minute) as its unit."""
    target_product_species = retrieve_product_species(hypo_end_stream)

    prod_run_mass = utils.unit_conversion_return_value_dq(target_product_species._run_mass, utils.UNIFIED_MASS_UNIT)
    residence_time = utils.unit_conversion_return_value_dq(hypo_reactor.residence_time, utils.UNIFIED_TIME_UNIT)
    reactor_volume = utils.unit_conversion_return_value_dq(hypo_reactor.reactor_volume, utils.UNIFIED_VOLUME_UNIT)

    _sty = prod_run_mass / residence_time / reactor_volume

    pi_sty = create_performance_indicator_instance(placeholder_perf_ind, _sty, utils.UNIFIED_SPACETIMEYIELD_UNIT)

    return pi_sty

def calculate_eco_score(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction eco score."""
    residence_time = utils.unit_conversion_return_value_dq(hypo_reactor.residence_time, utils.UNIFIED_TIME_UNIT)
    reactor_temperature = utils.unit_conversion_return_value_dq(hypo_reactor.reactor_temperature, OM_DEGREECELSIUS)
    time_temperature_eco_score = TIME_TEMPERATURE_ECO_SCORE_FACTOR * residence_time * (
        (reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS) * (reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS) / abs(reactor_temperature-AMBIENT_TEMPERATURE_DEGREECELSIUS))
    total_run_eco_score = retrieve_total_run_eco_score(hypo_reactor)
    _eco_score = ECO_SCORE_BASE_VALUE - time_temperature_eco_score - total_run_eco_score

    pi_eco_score = create_performance_indicator_instance(placeholder_perf_ind, _eco_score, utils.UNIFIED_ECOSCORE_UNIT)

    return pi_eco_score

def calculate_enviromental_factor(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction environmental factor."""
    target_product_species = retrieve_product_species(hypo_end_stream)
    all_reactant = [s for inlet in hypo_reactor.inlet_run_stream for s in inlet.solute if s._is_reactant]
    all_solvent = [inlet.solvent for inlet in hypo_reactor.inlet_run_stream]
    reactant_and_solvent = all_reactant + all_solvent

    total_reac_n_solvent_run_mass = sum([utils.unit_conversion_return_value_dq(s._run_mass, utils.UNIFIED_MASS_UNIT) for s in reactant_and_solvent])
    prod_run_mass = utils.unit_conversion_return_value_dq(target_product_species._run_mass, utils.UNIFIED_MASS_UNIT)

    _e_factor = prod_run_mass / (total_reac_n_solvent_run_mass - prod_run_mass)

    pi_e_factor = create_performance_indicator_instance(placeholder_perf_ind, _e_factor, utils.UNIFIED_ENVIRONMENTFACTOR_UNIT)

    return pi_e_factor

def calculate_run_material_cost(hypo_reactor: HypoReactor, hypo_end_stream: HypoEndStream, placeholder_perf_ind: PerformanceIndicator) -> PerformanceIndicator:
    """This method calculates the reaction material cost for a single run, which commonly has (pound sterling) as its unit."""
    all_reactant = [s for inlet in hypo_reactor.inlet_run_stream for s in inlet.solute if s._is_reactant]
    all_solvent = [inlet.solvent for inlet in hypo_reactor.inlet_run_stream]
    reactant_and_solvent = all_reactant + all_solvent
    # NOTE here the unit of the _run_volume and def_cost should already be standardised at creation of each HypoStreamSpecies instance
    # NOTE therefore the unit conversion is omitted
    _run_material_cost = sum([s._run_volume.hasNumericalValue * s.def_cost.hasNumericalValue for s in reactant_and_solvent])

    pi_run_material_cost = create_performance_indicator_instance(placeholder_perf_ind, _run_material_cost, utils.UNIFIED_RUN_MATERIAL_COST_UNIT)

    return pi_run_material_cost

def locate_placeholder_performance_indicator(rxn_exp_instance: ReactionExperiment, target_clz: str, expected_amount: int = None) -> PerformanceIndicator:
    """This method locates the placeholder PerformanceIndicator instance that already created in the ReactionExperiment instance."""
    lst_target = [pi for pi in rxn_exp_instance.hasPerformanceIndicator if pi.clz == target_clz]
    if expected_amount is None:
        return lst_target
    else:
        if len(lst_target) == expected_amount:
            return lst_target
        else:
            raise Exception("Identified amount of target PerformanceIndicator with a clz <%s> does NOT match the expected value (%d) in ReactionExperiment: %s" % (
                target_clz, expected_amount, str(rxn_exp_instance)))

def create_performance_indicator_instance(placeholder_instance: PerformanceIndicator, numerical_value, unit: str) -> PerformanceIndicator:
    """This method creates the instance of PerformanceIndicator given the placeholder instance and the computed value to be used for OM_Measure."""
    performance_indicator_instance = PerformanceIndicator(
        instance_iri=placeholder_instance.instance_iri,
        clz=placeholder_instance.clz,
        objPropWithExp=placeholder_instance.objPropWithExp,
        hasValue=OM_Measure(
            instance_iri=INSTANCE_IRI_TO_BE_INITIALISED,
            namespace_for_init=getNameSpace(placeholder_instance.instance_iri),
            hasUnit=unit,
            hasNumericalValue=numerical_value
        ),
        positionalID=placeholder_instance.positionalID
    )
    return performance_indicator_instance

def retrieve_yield_limiting_species(hypo_reactor: HypoReactor) -> HypoStreamSpecies:
    """This method retrieves the yield limiting reactant given the instance of HypoReactor."""
    all_inlet_stream = hypo_reactor.inlet_run_stream
    all_reactant_species = {r.species_iri:r for reac in all_inlet_stream for r in reac.solute if r._is_reactant}
    all_reactant_conc = {all_reactant_species[s].species_iri:all_reactant_species[s].run_conc for s in all_reactant_species}
    yield_limiting_conc = min([utils.unit_conversion_return_value_dq(all_reactant_conc[dq], utils.UNIFIED_CONCENTRATION_UNIT) for dq in all_reactant_conc])
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
    total_run_eco_score = sum([utils.unit_conversion_return_value_dq(s.def_eco_score, utils.UNIFIED_ECOSCORE_UNIT) for s in all_species])
    return total_run_eco_score
