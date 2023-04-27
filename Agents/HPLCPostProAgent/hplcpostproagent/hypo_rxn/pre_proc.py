from typing import Tuple

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.data_model.unit_conversion as unit_conv
import chemistry_and_robots.kg_operations.dict_and_list as dal

from hplcpostproagent.hypo_rxn.hypo_rxn import *

def calc_run_mol_n_volume_of_other_solute(
    input_chemical: str, species: str, eq_ratio: float,
    ref_solute_run_mol: unit_conv.DimensionalQuantity, pump_conc: unit_conv.DimensionalQuantity,
    dct_run_mol: dict, dct_run_volume: dict
):
    """This method calculates the run mol and run volume of any solute that are NOT in the reference pump."""
    _run_mol = eq_ratio * unit_conv.unit_conversion_dq(ref_solute_run_mol, unit_conv.UNIFIED_MOLE_UNIT).hasNumericalValue
    dal.deep_update(dct_run_mol, {input_chemical:{species:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MOLE_UNIT, hasNumericalValue=_run_mol)}})
    _run_volume = _run_mol / unit_conv.unit_conversion_dq(pump_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT).hasNumericalValue
    dal.deep_update(dct_run_volume, {input_chemical:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_VOLUME_UNIT, hasNumericalValue=_run_volume)})

def construct_hypo_end_stream(sparql_client: ChemistryAndRobotsSparqlClient, hplc_report_instance: HPLCReport, hypo_reactor: HypoReactor, species_role_dct: dict) -> HypoEndStream:
    """This method constructs an instance of HypoEndStream given the information about HPLCReport, HypoReactor, and the role of each species in the reaction."""
    lst_end_stream_comp = []
    _flag_for_unidentified_species = False # this is the flag to indicate if there're any unidentified chromatogram points
    # Iterate over the list of ChromatogramPoint and create instance of HypoStreamSpecies for each of them
    for pt in hplc_report_instance.records:
        if pt.indicatesComponent is None:
            _flag_for_unidentified_species = True
            continue
        else:
            # First determine the role of chemical species in the reaction
            _species_iri = pt.indicatesComponent.representsOccurenceOf
            try:
                _def_role = species_role_dct[_species_iri]
            except KeyError:
                raise Exception("Species <%s> identified from the HPLC report <%s> is NOT represented in the recording of ReactionExperiment <%s>" % (
                    _species_iri, hplc_report_instance.instance_iri, hypo_reactor.rxn_exp_iri))
            # Second query a list of "intrinsic" information related to the chemical species
            _mw = sparql_client.get_species_molar_mass_kilogrampermole(_species_iri)
            _density, _density_unit = sparql_client.get_species_density(_species_iri)
            _cost, _cost_unit = sparql_client.get_species_material_cost(_species_iri)
            _es, _es_unit = sparql_client.get_species_eco_score(_species_iri)
            # Third compute a list of information that derived from the "intrinsic" information
            _sp_run_conc, _sp_run_conc_unit = pt.indicatesComponent.hasProperty.hasValue.numericalValue, pt.indicatesComponent.hasProperty.hasValue.hasUnitOfMeasure
            _sp_run_mol = unit_conv.unit_conversion_return_value(_sp_run_conc, _sp_run_conc_unit,
                unit_conv.UNIFIED_CONCENTRATION_UNIT) * unit_conv.unit_conversion_return_value_dq(hypo_reactor.total_run_volume, unit_conv.UNIFIED_VOLUME_UNIT)
            # Fourth create the instance of HypoStreamSpecies based on the collected information
            end_stream_comp = HypoStreamSpecies(
                species_iri=_species_iri,
                def_role=_def_role,
                def_molar_mass=unit_conv.unit_conversion(_mw, OM_KILOGRAMPERMOLE, unit_conv.UNIFIED_MOLAR_MASS_UNIT),
                def_density=unit_conv.unit_conversion(_density, _density_unit, unit_conv.UNIFIED_DENSITY_UNIT),
                def_cost=unit_conv.unit_conversion(_cost, _cost_unit, unit_conv.UNIFIED_COST_UNIT),
                def_eco_score=unit_conv.unit_conversion(_es, _es_unit, unit_conv.UNIFIED_ECOSCORE_UNIT),
                run_conc=unit_conv.unit_conversion(_sp_run_conc, _sp_run_conc_unit, unit_conv.UNIFIED_CONCENTRATION_UNIT),
                run_mol=unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MOLE_UNIT,hasNumericalValue=_sp_run_mol)
            )
            lst_end_stream_comp.append(end_stream_comp)
    # Assemble the instance of HypoEndStream given all complete list of species
    hypo_end_stream = HypoEndStream(
        total_run_volume=unit_conv.unit_conversion_dq(hypo_reactor.total_run_volume, unit_conv.UNIFIED_VOLUME_UNIT),
        component=lst_end_stream_comp,
        containsUnidentifiedComponent=_flag_for_unidentified_species,
    )
    return hypo_end_stream

def construct_hypo_reactor(sparql_client: ChemistryAndRobotsSparqlClient, rxn_exp_instance: ReactionExperiment, internal_standard_instance: InternalStandard) -> Tuple[HypoReactor, float, dict]:
    """This method construct the instance of HypoReactor given the instance of ReactionExperiment and InternalStandard."""
    # Construct a dict of run volume from each pump (start with ReactionScale for the reference pump) in the format of {InputChemical:DimensionalQuantity(concentration)}
    _run_volume_dct = {con.indicatesUsageOf:unit_conv.DimensionalQuantity(hasUnit=con.hasValue.hasUnit,
        hasNumericalValue=con.hasValue.hasNumericalValue) for con in rxn_exp_instance.hasReactionCondition if con.indicatesUsageOf is not None}
    # Make sure only one InputChemical is identified as the reference pump
    if len(_run_volume_dct) > 1:
        raise Exception("Multiple ReactionScale conditions were found in the ReactionExperiment instance: %s" % str(rxn_exp_instance))
    elif len(_run_volume_dct) < 1:
        raise Exception("No ReactionScale condition was found in the ReactionExperiment instance: %s" % str(rxn_exp_instance))
    else:
        reference_input_chemical = list(_run_volume_dct.keys())[0]
        # Make the unit conversion from mL to L
        try:
            run_volume_dct = {reference_input_chemical:unit_conv.unit_conversion(_run_volume_dct[reference_input_chemical].hasNumericalValue,
                _run_volume_dct[reference_input_chemical].hasUnit, unit_conv.UNIFIED_VOLUME_UNIT)}
        except NotImplementedError:
            raise Exception("Unit conversion of ReactionScale from <%s> to <%s> is FAILED for ReactionExperiment: %s." % (
                _run_volume_dct[reference_input_chemical].hasUnit, unit_conv.UNIFIED_VOLUME_UNIT, str(rxn_exp_instance)))
        # TODO round((loopvol - 0.1), 1)???

    # Construct a dict of InputChemical
    input_chemical_dct = {ic.instance_iri:ic.thermodynamicBehaviour for ic in rxn_exp_instance.hasInputChemical}
    # Construct a dict of StoichiometryRatio
    stoi_ratio_dct = {con.indicatesMultiplicityOf:con.hasValue for con in rxn_exp_instance.hasReactionCondition if con.indicatesMultiplicityOf is not None}

    # Construct a dict of species role in the reaction
    lst_species = rxn_exp_instance.isOccurenceOf.get_list_of_occurring_species()
    species_role_dct = {us.hasUniqueSpecies:us.clz for us in lst_species}
    # Also add the internal standard to the dct
    if internal_standard_instance.representsOccurenceOf not in species_role_dct:
        dal.deep_update(species_role_dct, {internal_standard_instance.representsOccurenceOf:internal_standard_instance.clz})
    else:
        raise Exception("InternalStandard <%s> is already playing a role as <%s> in the ReactionExperiment <%s>" % (
            internal_standard_instance.representsOccurenceOf, species_role_dct.get(internal_standard_instance.representsOccurenceOf), rxn_exp_instance.instance_iri))

    # Construct the one-to-one identification of species in the input chemical, i.e. {InputChemical:{Role:Species}}
    species_role_in_input_chemical_dct = {ic:{species_role_dct[pc.representsOccurenceOf]:pc.representsOccurenceOf for pc in input_chemical_dct[ic].isComposedOfSubsystem} for ic in input_chemical_dct}
    # Check to make sure all species are presented in the constructed dct
    # NOTE we assume here only ONE appearance of each role in ONE InputChemical, e.g. we don't allow two species have role of Reactant within one InputChemical (pump inlet)
    for ic in species_role_in_input_chemical_dct:
        if len(species_role_in_input_chemical_dct[ic]) != len(input_chemical_dct[ic].isComposedOfSubsystem):
            raise Exception("Role of species (%s) within InputChemical <%s> is NOT uniquely presented: %s" % (
                str(input_chemical_dct[ic].isComposedOfSubsystem), ic, str(species_role_in_input_chemical_dct[ic])))

    # Construct a dict for pump concentration
    # species_pump_conc_dct = {"inputchemical":{"role":{"species":DimensionQuantity(concentration)}}}
    species_pump_conc_dct = {}
    for ic_key in input_chemical_dct:
        dal.deep_update(species_pump_conc_dct,
        {ic_key:{species_role_dct[p.representsOccurenceOf]:{p.representsOccurenceOf:unit_conv.DimensionalQuantity(hasUnit=p.hasProperty.hasValue.hasUnitOfMeasure,
            hasNumericalValue=p.hasProperty.hasValue.numericalValue)} for p in input_chemical_dct[ic_key].isComposedOfSubsystem}})

    # Construct a dct of reactant
    reactant_dct = {s:species_role_dct[s] for s in species_role_dct if species_role_dct[s] == ONTOKIN_REACTANT}

    # Construct a dct of catalyst
    catalyst_dct = {s:species_role_dct[s] for s in species_role_dct if species_role_dct[s] == ONTOREACTION_CATALYST}

    # Construct a dct of internal standard
    internal_standard_dct = {s:species_role_dct[s] for s in species_role_dct if species_role_dct[s] == ONTOHPLC_INTERNALSTANDARD}

    # Construct a dct of solvent
    solvent_dct = {s:species_role_dct[s] for s in species_role_dct if species_role_dct[s] == ONTOREACTION_SOLVENT}

    # Construct a dct of (reactant+catalyst) combined
    reactant_n_catalyst_dct = {**reactant_dct, **catalyst_dct}

    # Construct a dct of (reactant+catalyst+internal_standard) combined - this is equivalent to a dct of all possible solute in the pump inlet run stream
    solute_dct = {**reactant_dct, **catalyst_dct, **internal_standard_dct}

    # Get the reference solute (the reactant from the reference pump)
    reference_solute = species_role_in_input_chemical_dct[reference_input_chemical][ONTOKIN_REACTANT]

    # Calculate the run mol of reactant in the reference pump (reference InputChemical)
    # Get the unit for run mol correct
    run_mol_dct = {} # this dict will be populated in the format of {InputChemical:{Species:DimensionalQuantity(mol)}}
    # 1. First put the run mol of reactant in the reference pump
    try:
        _temp_value = unit_conv.unit_conversion_return_value(species_pump_conc_dct[reference_input_chemical][ONTOKIN_REACTANT][reference_solute].hasNumericalValue,
            species_pump_conc_dct[reference_input_chemical][ONTOKIN_REACTANT][reference_solute].hasUnit,
            unit_conv.UNIFIED_CONCENTRATION_UNIT) * unit_conv.unit_conversion_return_value(run_volume_dct[reference_input_chemical].hasNumericalValue,
            run_volume_dct[reference_input_chemical].hasUnit, unit_conv.UNIFIED_VOLUME_UNIT)
        dal.deep_update(run_mol_dct, {reference_input_chemical:{reference_solute:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MOLE_UNIT, hasNumericalValue=_temp_value)}})
    except NotImplementedError:
        raise Exception("The computation for reaction run mol using species concentration unit <%s> and liquid volume unit <%s> is FAILED while for reactant <%s> as part of InputChemical <%s>." % (
            species_pump_conc_dct[reference_input_chemical][ONTOKIN_REACTANT][reference_solute].hasUnit, run_volume_dct[reference_input_chemical].hasUnit,
            reference_solute, reference_input_chemical))

    # 2. Second calculate the run mol of all other solute in the InputChemical; also populate the run volume of each InputChemical (pump)
    for ic in stoi_ratio_dct:
        if ic != reference_input_chemical:
            if ONTOKIN_REACTANT in species_role_in_input_chemical_dct[ic]:
                _temp_role = ONTOKIN_REACTANT
            elif ONTOREACTION_CATALYST in species_role_in_input_chemical_dct[ic]:
                _temp_role = ONTOREACTION_CATALYST
            _temp_species = species_role_in_input_chemical_dct[ic][_temp_role]
            calc_run_mol_n_volume_of_other_solute(input_chemical=ic, species=_temp_species, eq_ratio=stoi_ratio_dct[ic].hasNumericalValue,
                ref_solute_run_mol=run_mol_dct[reference_input_chemical][reference_solute], pump_conc=species_pump_conc_dct[ic][_temp_role][_temp_species],
                dct_run_mol=run_mol_dct, dct_run_volume=run_volume_dct)

    # 3. Third, calculate the run mol of the internal standard in the InputChemical (this means that internal standard can be presented in any pump)
    # 4. Forth, at the same time calculate the run mol of all solvent
    for ic in run_volume_dct:
        if ONTOHPLC_INTERNALSTANDARD in species_role_in_input_chemical_dct[ic]:
            _temp_role = ONTOHPLC_INTERNALSTANDARD
            _temp_species = species_role_in_input_chemical_dct[ic][_temp_role]
            _is_run_mol = unit_conv.unit_conversion_return_value_dq(species_pump_conc_dct[ic][_temp_role][_temp_species], unit_conv.UNIFIED_CONCENTRATION_UNIT) * run_volume_dct[ic].hasNumericalValue
            dal.deep_update(run_mol_dct, {ic:{_temp_species:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MOLE_UNIT, hasNumericalValue=_is_run_mol)}})
        if ONTOREACTION_SOLVENT in species_role_in_input_chemical_dct[ic]:
            _temp_role = ONTOREACTION_SOLVENT
            _temp_species = species_role_in_input_chemical_dct[ic][_temp_role]
            _solvent_run_mol = unit_conv.unit_conversion_return_value_dq(species_pump_conc_dct[ic][_temp_role][_temp_species], unit_conv.UNIFIED_CONCENTRATION_UNIT) * run_volume_dct[ic].hasNumericalValue
            dal.deep_update(run_mol_dct, {ic:{_temp_species:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_MOLE_UNIT, hasNumericalValue=_solvent_run_mol)}})

    # Calculate the total_run_volume
    total_run_volume = unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_VOLUME_UNIT,
        hasNumericalValue=sum([unit_conv.unit_conversion_return_value_dq(dq, unit_conv.UNIFIED_VOLUME_UNIT) for dq in list(run_volume_dct.values())]))
    # Calculate the run conc of all species: mol/total_run_volume
    # Also, create a list of HypoStreamSpecies
    run_conc_dct = {} # {InputChemical:{Species:DimensionalQuantity(concentration)}}
    lst_inlet_stream = []
    _dct_IS_run_conc_moleperlitre = {}
    for ic in run_volume_dct:
        lst_solute = []
        lst_solvent = []
        for _role in species_role_in_input_chemical_dct[ic]:
            _species_iri = species_role_in_input_chemical_dct[ic][_role]
            _sp_run_conc = run_mol_dct[ic][_species_iri].hasNumericalValue / total_run_volume.hasNumericalValue
            dal.deep_update(run_conc_dct, {ic:{_species_iri:unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_CONCENTRATION_UNIT,hasNumericalValue=_sp_run_conc)}})

            # Append the run_conc of InternalStandard to save computation at later stage when returning the value
            if _role == ONTOHPLC_INTERNALSTANDARD:
                dal.deep_update(_dct_IS_run_conc_moleperlitre, {_species_iri:unit_conv.unit_conversion_return_value(_sp_run_conc, unit_conv.UNIFIED_CONCENTRATION_UNIT, OM_MOLEPERLITRE)})

            _mw = sparql_client.get_species_molar_mass_kilogrampermole(_species_iri)
            _density, _density_unit = sparql_client.get_species_density(_species_iri)
            _cost, _cost_unit = sparql_client.get_species_material_cost(_species_iri)
            _es, _es_unit = sparql_client.get_species_eco_score(_species_iri)
            hypo_species = HypoStreamSpecies(
                species_iri=_species_iri,
                def_role=species_role_dct[_species_iri],
                def_molar_mass=unit_conv.unit_conversion(_mw, OM_KILOGRAMPERMOLE, unit_conv.UNIFIED_MOLAR_MASS_UNIT),
                def_density=unit_conv.unit_conversion(_density, _density_unit, unit_conv.UNIFIED_DENSITY_UNIT),
                def_cost=unit_conv.unit_conversion(_cost, _cost_unit, unit_conv.UNIFIED_COST_UNIT),
                def_eco_score=unit_conv.unit_conversion(_es, _es_unit, unit_conv.UNIFIED_ECOSCORE_UNIT),
                run_conc=unit_conv.DimensionalQuantity(hasUnit=unit_conv.UNIFIED_CONCENTRATION_UNIT,hasNumericalValue=_sp_run_conc),
                run_mol=unit_conv.DimensionalQuantity(hasUnit=run_mol_dct[ic][_species_iri].hasUnit,hasNumericalValue=run_mol_dct[ic][_species_iri].hasNumericalValue)
            )
            if species_role_dct[_species_iri] == ONTOREACTION_SOLVENT:
                lst_solvent.append(hypo_species)
            else:
                lst_solute.append(hypo_species)
        if len(lst_solvent) > 1:
            raise Exception("More than ONE instance of OntoRxn:Solvent identified for InputChemical instance <%s>: %s" % (ic, str(lst_solvent)))
        elif len(lst_solvent) < 1:
            raise Exception("No instance of OntoRxn:Solvent identified for InputChemical instance <%s>" % ic)
        else:
            solvent_ins = lst_solvent[0]
        hypo_pump_inlet = HypoPumpRunStream(
            is_ref_pump=True if ic == reference_input_chemical else False,
            inlet_iri=ic,
            run_volume=unit_conv.DimensionalQuantity(hasUnit=run_volume_dct[ic].hasUnit, hasNumericalValue=run_volume_dct[ic].hasNumericalValue),
            eq_ratio=unit_conv.DimensionalQuantity(hasUnit=stoi_ratio_dct[ic].hasUnit,hasNumericalValue=stoi_ratio_dct[ic].hasNumericalValue),
            solute=lst_solute,
            solvent=solvent_ins
        )
        lst_inlet_stream.append(hypo_pump_inlet)

    # Collect information on reactor volume and residence time
    _reactor_vol, _reactor_vol_unit = sparql_client.get_reactor_volume_given_reactor(rxn_exp_instance.isAssignedTo)
    _res_time_con = [con for con in rxn_exp_instance.hasReactionCondition if con.clz == ONTOREACTION_RESIDENCETIME]
    if len(_res_time_con) > 1:
        raise Exception("More than ONE instance of OntoRxn:ResidenceTime identified as ReactionCondition within ReactionExperiment instance <%s>: %s" % (
            rxn_exp_instance.instance_iri, str(_res_time_con)))
    elif len(_res_time_con) < 1:
        raise Exception("No instance of OntoRxn:ResidenceTime identified as ReactionCondition within ReactionExperiment instance <%s>" % rxn_exp_instance.instance_iri)
    else:
        _res_time = _res_time_con[0].hasValue.hasNumericalValue
        _res_time_unit = _res_time_con[0].hasValue.hasUnit

    # NOTE TODO here we assume the ReactionTemperature is the actual temperature within the reactor, ideally we will use the actual data collected from sensors if possible
    _rxn_temp_con = [con for con in rxn_exp_instance.hasReactionCondition if con.clz == ONTOREACTION_REACTIONTEMPERATURE]
    if len(_rxn_temp_con) > 1:
        raise Exception("More than ONE instance of OntoRxn:ReactionTemperature identified as ReactionCondition within ReactionExperiment instance <%s>: %s" % (
            rxn_exp_instance.instance_iri, str(_rxn_temp_con)))
    elif len(_rxn_temp_con) < 1:
        raise Exception("No instance of OntoRxn:ReactionTemperature identified as ReactionCondition within ReactionExperiment instance <%s>" % rxn_exp_instance.instance_iri)
    else:
        _rxn_temp = _rxn_temp_con[0].hasValue.hasNumericalValue
        _rxn_temp_unit = _rxn_temp_con[0].hasValue.hasUnit

    # Construct the HypoReactor instance
    hypo_reactor = HypoReactor(
        rxn_exp_iri=rxn_exp_instance.instance_iri,
        residence_time=unit_conv.unit_conversion(_res_time, _res_time_unit, unit_conv.UNIFIED_TIME_UNIT),
        reactor_temperature=unit_conv.unit_conversion(_rxn_temp, _rxn_temp_unit, unit_conv.UNIFIED_TEMPERATURE_UNIT),
        reactor_volume=unit_conv.unit_conversion(_reactor_vol, _reactor_vol_unit, unit_conv.UNIFIED_VOLUME_UNIT),
        inlet_run_stream=lst_inlet_stream,
        total_run_volume=unit_conv.unit_conversion_dq(total_run_volume, unit_conv.UNIFIED_VOLUME_UNIT)
    )

    # Make sure there is only one instance of run concentration for InternalStandard
    if len(_dct_IS_run_conc_moleperlitre) > 1:
        raise Exception("Multiple appearances of InternalStandard (%s) in the given reaction: %s" % (str(_dct_IS_run_conc_moleperlitre), str(rxn_exp_instance)))
    elif len(_dct_IS_run_conc_moleperlitre) < 1:
        raise Exception("No appearance of InternalStandard in the given reaction: %s" % (str(rxn_exp_instance)))
    else:
        internal_standard_run_conc_moleperlitre = list(_dct_IS_run_conc_moleperlitre.values())[0]

    return hypo_reactor, internal_standard_run_conc_moleperlitre, species_role_dct
