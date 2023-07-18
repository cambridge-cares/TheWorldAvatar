from summit.utils.dataset import DataSet as DataSet_summit
from summit.domain import ContinuousVariable as ContinuousVariable_summit
from summit.domain import CategoricalVariable as CategoricalVariable_summit
from summit.domain import Domain as Domain_summit
from summit.strategies import TSEMO as TSEMO_summit

from .new_stbo import NewSTBO as NewSTBO_summit

from functools import reduce
from typing import List
import pandas as pd

import doeagent.data_model as dm
import doeagent.kg_operations as kg

from py4jps import agentlogging
logger = agentlogging.get_logger('prod')

TWO_POWER_31_MINUS_ONE = 2**31-1
SYS_RES_LOWER_BOUND = -TWO_POWER_31_MINUS_ONE
SYS_RES_UPPER_BOUND = TWO_POWER_31_MINUS_ONE

def proposeNewExperiment(
    doe: dm.DesignOfExperiment,
    sparql_client: kg.ChemistryAndRobotsSparqlClient,
    lab_iri: str = None,
) -> List[dm.ReactionExperiment]:
    """
        This method is a wrapper around the TSEMO algorithm as provided by python package `summit`. It suggests the new experiment given information about DesignOfExperiment.
        
        Arguments:
            doe - instance of dataclass OntoDoE.DesignOfExperiment
                    Stores information about design of experiment exercise retrieved from the OntoDoE:DesignOfExperiment instance in the knowledge graph
    """
    # Create domain for design of experiment
    domain = Domain_summit()

    # Add all optimisation variables to domain
    for var in doe.hasDomain.hasDesignVariable:
        if isinstance(var, dm.CategoricalVariable):
            domain += CategoricalVariable_summit(
                name=var.name, description=var.instance_iri,
                levels=var.hasLevel
            )
        elif isinstance(var, dm.ContinuousVariable):
            domain += ContinuousVariable_summit(
                name=var.name, description=var.instance_iri,
                bounds=[var.lowerLimit, var.upperLimit]
            )
    # Add all system responses to domain
    for var in doe.hasSystemResponse:
        domain += ContinuousVariable_summit(
            name=var.name, description=var.instance_iri,
            bounds=[SYS_RES_LOWER_BOUND, SYS_RES_UPPER_BOUND], is_objective=True, maximize=var.maximise
        )

    # Create strategy (only supporting TSEMO at the moment)
    # TODO support LHS and other algorithms
    if isinstance(doe.usesStrategy, dm.TSEMO):
        strategy = TSEMO_summit(
            domain,
            n_spectral_points=doe.usesStrategy.nSpectralPoints,
            generations=doe.usesStrategy.nGenerations,
            n_retries=doe.usesStrategy.nRetries,
            pop_size=doe.usesStrategy.populationSize
            )
    elif isinstance(doe.usesStrategy, dm.NewSTBO):
        strategy = NewSTBO_summit(domain)
    else:
        raise Exception('Currently only TSEMO and NewSTBO is supported as DoE algorithm.')

    # Construct table of historical data "previous_results"
    # The "previous_results" is a dataframe looks like below:
    #    | ContinuousVariable_1 | ContinuousVariable_2 | ContinuousVariable_3 | ContinuousVariable_4 | SystemResponse_1 | SystemResponse_2
    # 0  |                5.19  |                0.10  |                14.7  |                42.0  |            47.9  |            7.44
    # 1  |                1.59  |                0.07  |                13.3  |                35.0  |             8.7  |            7.74
    # 2  |                8.44  |                0.16  |                 7.9  |                62.0  |            54.1  |            6.96
    # 3  |                8.83  |                0.04  |                11.8  |                67.0  |            40.0  |            8.10
    # 4  |                5.01  |                0.17  |                 8.1  |                56.0  |            47.7  |            6.83
    previous_results = constructPreviousResultsTable(doe) if doe.utilisesHistoricalData.refersToExperiment is not None else None

    # Suggest the next experiment, the output "next_exp" is a DataSet contains the suggested values for the optimisation variables in the next runs
    next_exp = strategy.suggest_experiments(doe.utilisesHistoricalData.numOfNewExp, prev_res=previous_results)

    # Extract the suggestion from Summit DataSet and populate that to dataclass ontorxn.ReactionExperiment/ReactionVariation
    new_rxn_exp = formNewExperiment(doe, next_exp, sparql_client, lab_iri)

    # Return the new experiment
    # NOTE here we slice the list to only return the amount of experiments requested
    # NOTE this is due to the fact that TSEMO will return 2 experiments even if only 1 is requested when there's no prior experiment data
    return new_rxn_exp[:doe.utilisesHistoricalData.numOfNewExp]

def formNewExperiment(
    doe: dm.DesignOfExperiment,
    new_exp_ds: DataSet_summit,
    sparql_client: kg.ChemistryAndRobotsSparqlClient,
    lab_iri: str = None,
) -> List[dm.ReactionExperiment]:
    """
        This method converts the Summit suggested experiment from DataSet format to an instance of dataclass ontodoe.NewExperiment.
    """
    # Initialise a list to host the new suggested ReactionExperiment/ReactionVariation instances
    list_rxnvar = []

    # Locate the possible input chemicals for each of the stoichiometric ratio and reaction scale
    # This is done irrespective of whether the reaction is a ReactionExperiment or ReactionVariation
    # So that this enables operation over multiple labs
    # TODO [urgent] pass a list of chemical species IRI to the function
    # TODO [next iteration] unify the way of handling continuous and categorical variables for stoichiometry ratio and reaction scale
    _stoi_ratio_list = [
        var for var in doe.hasDomain.hasDesignVariable if isinstance(var, dm.ContinuousVariable) and var.refersToQuantity.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO
    ] + [
        param for param in doe.hasDomain.hasFixedParameter if param.refersToQuantity.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO
    ]
    for var in doe.hasDomain.hasDesignVariable:
        if isinstance(var, dm.CategoricalVariable) and var.refersToQuantity.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO:
            var.positionalID = new_exp_ds[var.name].iloc[0]
            _stoi_ratio_list.append(var)
    # NOTE that here we assume the ReactionScale has the same positionalID as the StoichiometryRatio of the reference chemical
    _rxn_scale_list = [
        var for var in doe.hasDomain.hasDesignVariable if var.refersToQuantity.clz == dm.ONTOREACTION_REACTIONSCALE
    ] + [
        param for param in doe.hasDomain.hasFixedParameter if param.refersToQuantity.clz == dm.ONTOREACTION_REACTIONSCALE
    ]
    _rxn_scale_id_list = [_rs.positionalID for _rs in _rxn_scale_list]
    if not all([_rs.positionalID in [_sr.positionalID for _sr in _stoi_ratio_list] for _rs in _rxn_scale_list]):
        raise Exception(f"The ReactionScale {_rxn_scale_list} is not identifying any of the chemicals pointed by StoichiometryRatio {_stoi_ratio_list}.")

    chemical_reaction_instance = sparql_client.get_chemical_reaction_given_iri(doe.designsChemicalReaction)
    _solvent = chemical_reaction_instance.get_list_of_solvent()
    _product = chemical_reaction_instance.get_list_of_product()

    _input_chemical_dict = {stoi.positionalID:sparql_client.locate_possible_input_chemical(
        solute=stoi.positionalID,
        solvent_as_constraint=_solvent,
        species_to_exclude=_product,
        list_of_labs_as_constraint=[lab_iri] if lab_iri else None,
        is_ref_chemical=True if stoi.positionalID in _rxn_scale_id_list else False,
    ) for stoi in _stoi_ratio_list}
    logger.debug(f"Input chemical dict: {_input_chemical_dict}")

    # Iterate over the new suggested experiments to create each of them
    # NOTE below design works for multiple (>1) experiments
    # NOTE however, for the time being, the DoE Agent will be used to generate 1 experiment to fit the derivation framework
    # NOTE i.e. len(new_exp_ds) == 1
    # NOTE here we iterate through index (instead of range(len(new_exp_ds))) to make it robust against the situation where index doesn't start from 0
    for i in new_exp_ds.index:
        # Get the first ReactionExperiment in the historical data if it exists
        # The new created ReactionVariation instances <isVariationOf> this ReactionExperiment
        # Most of the information from this ReactionExperiment will be copied to the new created ReactionVariation instance
        # NOTE the ReactionVariation MUST and ONLY <isVariationOf> instance of ReactionExperiment, otherwise it will create huge overhead in recursive querying function getReactionExperiment
        if doe.utilisesHistoricalData.refersToExperiment is not None:
            first_rxn_exp = [rxn_exp for rxn_exp in doe.utilisesHistoricalData.refersToExperiment if rxn_exp.clz == dm.ONTOREACTION_REACTIONEXPERIMENT][0]

            # Prepare a list of ReactionCondition
            list_con = []
            # Iterate over ReactionCondition in parent ReactionExperiment to populate the new suggested ReactionCondition in ReactionVariation
            # The ReactionCondition kept unchanged will be preserved (new instance of ReactionCondition will be generated)
            for first_rxn_exp_con in first_rxn_exp.hasReactionCondition:
                _var = []
                var_loc = []
                for design_var in doe.hasDomain.hasDesignVariable:
                    if isinstance(design_var, dm.ContinuousVariable):
                        if tuple((design_var.refersToQuantity.clz, design_var.positionalID)) == tuple((first_rxn_exp_con.clz, first_rxn_exp_con.positionalID)):
                            _var.append(design_var)
                            var_loc.append(design_var.name)
                    elif isinstance(design_var, dm.CategoricalVariable):
                        if design_var.refersToQuantity.clz == first_rxn_exp_con.clz and first_rxn_exp_con.positionalID in design_var.hasLevel:
                            _var.append(design_var)
                            var_loc.append(design_var.name)

                if len(var_loc) > 1:
                    raise Exception(
                            """Only one appearance should be allowed for a ReactionCondition to be a DesignVariable within one ReactionExperiment/ReactionVariation. \
                            If you intend to use ReactionCondition that instantiated from same class for different variables, please consider use positionalID to distinguish.
                            """)
                if len(var_loc) < 0:
                    # it means this ReactionCondition is not adjusted in the DoE, so should be kept as the same value in the parent ReactionExperiment
                    # TODO if we don't want to add duplicated data to KG, we may 'continue' here, and add support in querying ReactionVariation to retrieve data from its parent ReactionExperiment as well
                    # TODO in that case, line 103 "hasNumericalValue=first_rxn_exp_con.hasValue.hasNumericalValue if len(var_loc) < 1 else new_exp_ds[var_loc[0]][i]"
                    # TODO should be updated to "hasNumericalValue=new_exp_ds[var_loc[0]][i]" as "len(var_loc) > 0" is guaranteed (as we 'continue' if true)
                    pass

                logger.debug("-------------------------------------------------------------------------------------")
                logger.debug("New suggested experiment summit DataSet:")
                logger.debug(new_exp_ds)
                logger.debug("-------------------------------------------------------------------------------------")

                # Prepare numerical value for the OM_Measure
                # NOTE TODO here we took a short-cut wrt decimal places, in the future, this should be connected to KG
                _raw_numerical_value_ = first_rxn_exp_con.hasValue.hasNumericalValue if len(var_loc) < 1 or isinstance(_var[0], dm.CategoricalVariable) else new_exp_ds[var_loc[0]][i] # an example: df['ContinuousVariable_1'][0]
                try:
                    _decimal_place = dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT[first_rxn_exp_con.clz]
                except KeyError:
                    raise Exception(f"Decimal places for {first_rxn_exp_con.clz} is not defined in ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT {dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT}.")

                try:
                    _decimal_numerical_val = round(_raw_numerical_value_, _decimal_place) if _decimal_place > 0 else int(_raw_numerical_value_)
                except TypeError:
                    raise Exception(f"Cannot round {_raw_numerical_value_} to {_decimal_place} decimal places.")

                # Create instance for OM_Measure
                om_measure = dm.OM_Measure(
                    instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=dm.getNameSpace(first_rxn_exp_con.hasValue.instance_iri),
                    hasUnit=first_rxn_exp_con.hasValue.hasUnit,
                    # TODO for the moment, a new om:Measure instance is always created
                    hasNumericalValue=_decimal_numerical_val
                )

                # Create instance for ReactionCondition
                _objPropWithExp = first_rxn_exp_con.objPropWithExp
                if dm.ONTOREACTION_HASREACTIONCONDITION not in _objPropWithExp:
                    _objPropWithExp.append(dm.ONTOREACTION_HASREACTIONCONDITION)

                # TODO [next iteration] provide better handle for the case where the ReactionCondition is not a DesignVariable/is a CategoricalVariable/is a ContinuousVariable
                _ref_var = first_rxn_exp_con if len(var_loc) < 1 else _var[0]
                con = dm.ReactionCondition(
                    instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=dm.getNameSpace(first_rxn_exp_con.instance_iri),
                    clz=first_rxn_exp_con.clz,
                    objPropWithExp=_objPropWithExp,
                    hasValue=om_measure,
                    positionalID=_ref_var.positionalID,
                    indicatesMultiplicityOf=_input_chemical_dict[_ref_var.positionalID].instance_iri if first_rxn_exp_con.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO else None,
                    indicatesUsageOf=_input_chemical_dict[_ref_var.positionalID].instance_iri if first_rxn_exp_con.clz == dm.ONTOREACTION_REACTIONSCALE else None,
                )

                # Add created instance to list
                list_con.append(con)

            # Populate the information to create instance of ReactionVariation
            rxnvar = dm.ReactionVariation(
                instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=dm.getNameSpace(first_rxn_exp.instance_iri),
                hasReactionCondition=list_con,
                hasPerformanceIndicator=None,
                hasInputChemical=[_input_chemical_dict[ic] for ic in _input_chemical_dict],# this should be decided for each reaction variation as well
                # NOTE here the OutputChemical is set to be None as the OutputChemical will need to be generated after the physical experimentation
                hasOutputChemical=None,
                isVariationOf=first_rxn_exp
            )

            # Add created instance to list of ReactionVariation
            list_rxnvar.append(rxnvar)

        else:
            # doe.utilisesHistoricalData.refersToExperiment is None, therefore, all information should be based on DoE
            # the list of ReactionCondition either from the suggested values for each DesignVariable
            # or from the fixed pamameters of DoE
            list_con = []

            # first, we iterate over the design variables
            for design_var in doe.hasDomain.hasDesignVariable:
                if isinstance(design_var, dm.ContinuousVariable):
                    # Prepare numerical value for the OM_Measure
                    # NOTE TODO here we took a short-cut wrt decimal places, in the future, this should be connected to KG
                    _raw_numerical_value_ = new_exp_ds[design_var.name][i]

                    try:
                        _decimal_place = dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT[design_var.refersToQuantity.clz]
                    except KeyError:
                        raise Exception(f"Decimal places for {design_var.refersToQuantity.clz} is not defined in ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT {dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT}.")

                    _decimal_numerical_val = round(_raw_numerical_value_, _decimal_place) if _decimal_place > 0 else int(_raw_numerical_value_)

                    # Create instance for OM_Measure
                    om_measure = dm.OM_Measure(
                        instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=dm.getNameSpace(design_var.instance_iri),
                        hasUnit=design_var.refersToQuantity.hasUnit,
                        # TODO for the moment, a new om:Measure instance is always created
                        hasNumericalValue=_decimal_numerical_val
                    )
                else:
                    # for the design variable is a categorical variable
                    om_measure = dm.OM_Measure(
                        instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                        namespace_for_init=dm.getNameSpace(design_var.instance_iri),
                        hasUnit=design_var.refersToQuantity.hasValue.hasUnit,
                        hasNumericalValue=design_var.refersToQuantity.hasValue.hasNumericalValue
                    )

                # Create instance for ReactionCondition
                _objPropWithExp = dm.OBJECT_RELATIONSHIP_REACTION_CONDITION_RXN_EXP_DICT[design_var.refersToQuantity.clz]
                con = dm.ReactionCondition(
                    instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=dm.getNameSpace(design_var.instance_iri),
                    clz=design_var.refersToQuantity.clz,
                    objPropWithExp=_objPropWithExp,
                    hasValue=om_measure,
                    positionalID=design_var.positionalID,
                    indicatesMultiplicityOf=_input_chemical_dict[design_var.positionalID].instance_iri if design_var.refersToQuantity.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO else None,
                    indicatesUsageOf=_input_chemical_dict[design_var.positionalID].instance_iri if design_var.refersToQuantity.clz == dm.ONTOREACTION_REACTIONSCALE else None,
                )
                list_con.append(con)

            for fixed_param in doe.hasDomain.hasFixedParameter:
                # Prepare numerical value for the OM_Measure
                # NOTE TODO here we took a short-cut wrt decimal places, in the future, this should be connected to KG
                # NOTE if the fixed parameter is a reaction scale, then we need to use the value of the recommended reaction scale
                if fixed_param.refersToQuantity.clz == dm.ONTOREACTION_REACTIONSCALE:
                    _recommended_reaction_scale = _input_chemical_dict[fixed_param.positionalID].recommended_reaction_scale
                    if _recommended_reaction_scale is not None:
                        _raw_numerical_value_ = _recommended_reaction_scale.hasValue.hasNumericalValue
                        _unit = _recommended_reaction_scale.hasValue.hasUnit
                    else:
                        _raw_numerical_value_ = fixed_param.refersToQuantity.hasValue.hasNumericalValue
                        _unit = fixed_param.refersToQuantity.hasValue.hasUnit
                else:
                    _raw_numerical_value_ = fixed_param.refersToQuantity.hasValue.hasNumericalValue
                    _unit = fixed_param.refersToQuantity.hasValue.hasUnit

                try:
                    _decimal_place = dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT[fixed_param.refersToQuantity.clz]
                except KeyError:
                    raise Exception(f"Decimal places for {fixed_param.refersToQuantity.clz} is not defined in ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT {dm.ROUND_DECIMAL_PLACES_REACTION_CONDITION_RXN_EXP_DICT}.")

                _decimal_numerical_val = round(_raw_numerical_value_, _decimal_place) if _decimal_place > 0 else int(_raw_numerical_value_)

                # Create instance for OM_Measure
                om_measure = dm.OM_Measure(
                    instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=dm.getNameSpace(fixed_param.instance_iri),
                    hasUnit=_unit,
                    # TODO for the moment, a new om:Measure instance is always created
                    hasNumericalValue=_decimal_numerical_val
                )

                # Create instance for ReactionCondition
                _objPropWithExp = dm.OBJECT_RELATIONSHIP_REACTION_CONDITION_RXN_EXP_DICT[fixed_param.refersToQuantity.clz]
                con = dm.ReactionCondition(
                    instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                    namespace_for_init=dm.getNameSpace(fixed_param.instance_iri),
                    clz=fixed_param.refersToQuantity.clz,
                    objPropWithExp=_objPropWithExp,
                    hasValue=om_measure,
                    positionalID=fixed_param.positionalID,
                    indicatesMultiplicityOf=_input_chemical_dict[fixed_param.positionalID].instance_iri if fixed_param.refersToQuantity.clz == dm.ONTOREACTION_STOICHIOMETRYRATIO else None,
                    indicatesUsageOf=_input_chemical_dict[fixed_param.positionalID].instance_iri if fixed_param.refersToQuantity.clz == dm.ONTOREACTION_REACTIONSCALE else None,
                )
                list_con.append(con)

            # Create instance for ReactionExperiment
            a_rxn_exp = dm.ReactionExperiment(
                instance_iri=dm.INSTANCE_IRI_TO_BE_INITIALISED,
                namespace_for_init=dm.getNameSpace(doe.instance_iri),
                hasReactionCondition=list_con,
                hasPerformanceIndicator=None,
                hasInputChemical=[_input_chemical_dict[ic] for ic in _input_chemical_dict],
                hasOutputChemical=None,
                isOccurenceOf=sparql_client.get_chemical_reaction_given_iri(doe.designsChemicalReaction)
            )
            list_rxnvar.append(a_rxn_exp)

    return list_rxnvar

def constructPreviousResultsTable(doe: dm.DesignOfExperiment) -> DataSet_summit:
    """
        This method constructs a summit.utils.dataset.DataSet instance from instance of dataclass OntoDoE.DesignOfExperiment 
        
        Arguments:
            doe - instance of dataclass OntoDoE.DesignOfExperiment
                    Stores information about design of experiment exercise retrieved from the OntoDoE:DesignOfExperiment instance in the knowledge graph \n
                    
                    The output "previous_results" is a dataframe that looks like below:
                       | ContinuousVariable_1 | ContinuousVariable_2 | ContinuousVariable_3 | ContinuousVariable_4 | SystemResponse_1 | SystemResponse_2 \n
                    0  |                5.19  |                0.10  |                14.7  |                42.0  |            47.9  |            7.44  \n
                    1  |                1.59  |                0.07  |                13.3  |                35.0  |             8.7  |            7.74  \n
                    2  |                8.44  |                0.16  |                 7.9  |                62.0  |            54.1  |            6.96  \n
                    3  |                8.83  |                0.04  |                11.8  |                67.0  |            40.0  |            8.10  \n
                    4  |                5.01  |                0.17  |                 8.1  |                56.0  |            47.7  |            6.83  \n
    """

    # Initialise the list of dict for historical data that will be turned into pandas.DataFrame
    list_of_prev_result_df = []

    list_cont_var = []
    list_cat_var = []
    list_sys_res = []

    # get all data for DesignVariable
    for var in doe.hasDomain.hasDesignVariable:
        # prepare data for the previous results table
        data = []
        for exp in doe.utilisesHistoricalData.refersToExperiment:
            if isinstance(var, dm.ContinuousVariable):
                list_cont_var.append(var.name)
                # locate the value of the DesignVariable in each historical experiment
                con = exp.get_reaction_condition(var.refersToQuantity.clz, var.positionalID)

                if con is None:
                    raise Exception(f"No ReactionCondition found for the DesignVariable (refersToQuantity clz: {var.refersToQuantity.clz} and positionalID: {var.positionalID}) in the historical experiment (instance_iri: {exp.instance_iri})")

                # append the collected value in the experiment
                data.append({'rxnexp': exp.instance_iri, var.name: con.hasValue.hasNumericalValue})

            elif isinstance(var, dm.CategoricalVariable):
                list_cat_var.append(var.name)
                con_list = [exp.get_reaction_condition(var.refersToQuantity.clz, level) for level in var.hasLevel]
                con_list_non_none = [con for con in con_list if con is not None]

                if len(con_list_non_none) == 0:
                    raise Exception(f"No ReactionCondition found for the DesignVariable (refersToQuantity clz: {var.refersToQuantity.clz} and categorical level: {var.hasLevel}) in the historical experiment (instance_iri: {exp.instance_iri})")
                elif len(con_list_non_none) > 1:
                    raise Exception(f"Multiple ReactionCondition found for the DesignVariable (refersToQuantity clz: {var.refersToQuantity.clz} and categorical level: {var.hasLevel}) in the historical experiment (instance_iri: {exp.instance_iri})")
                else:
                    data.append({'rxnexp': exp.instance_iri, var.name: con_list_non_none[0].positionalID})

            else:
                raise NotImplementedError(f"DesignVariable type: {type(var)} not implemented yet.")

        # the prepared data will be converted from a dict to a pandas.DataFrame and added to a list
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_prev_result_df.append(pd.DataFrame.from_dict(_to_df))

    # get all data for SystemResponse
    for var in doe.hasSystemResponse:
        list_sys_res.append(var.name)
        # prepare data for the previous results table
        data = []
        for exp in doe.utilisesHistoricalData.refersToExperiment:
            # locate the value of the SystemResponse in each historical experiment
            indi = exp.get_performance_indicator(var.refersToQuantity, var.positionalID)

            if indi is None:
                raise Exception(f"No PerformanceIndicator found for the SystemResponse (refersToQuantity clz: {var.refersToQuantity} and positionalID: {var.positionalID}) in the historical experiment (instance_iri: {exp.instance_iri})")

            # append the collected value in the experiment
            # NOTE here we clip the value to be within the range of [SYS_RES_LOWER_BOUND, SYS_RES_UPPER_BOUND], i.e. [-2**31-1, 2**31-1]
            # this fixes the bug of not able to handle -inf and inf values
            data.append({'rxnexp': exp.instance_iri, var.name: max(SYS_RES_LOWER_BOUND, min(indi.hasValue.hasNumericalValue, SYS_RES_UPPER_BOUND))})
        # the prepared data will be converted from a dict to a pandas.DataFrame and added to a list
        _to_df = {}
        for k in data[0]:
            _to_df[k] = tuple(d[k] for d in data)
        list_of_prev_result_df.append(pd.DataFrame.from_dict(_to_df))

    # Merge the list of pandas.DataFrame to one DataFrame, using the IRI of OntoRxn:ReactionExperiment as unique identifier
    previousResults_df = reduce(lambda df1, df2: pd.merge(df1, df2, on='rxnexp'), list_of_prev_result_df)

    # Drop the column "rxnexp" as it is not needed anymore
    previousResults_df = previousResults_df.drop(columns="rxnexp")

    # Convert the continuous variables and system responses to float
    previousResults_df = previousResults_df.astype({v: float for v in list_cont_var})
    previousResults_df = previousResults_df.astype({r: float for r in list_sys_res})
    # Convert the categorical variables to str
    previousResults_df = previousResults_df.astype({c: str for c in list_cat_var})

    previous_results = DataSet_summit.from_df(previousResults_df)

    return previous_results
