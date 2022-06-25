import uuid
from chemistry_and_robots.tests.conftest import TargetIRIs
import chemistry_and_robots.tests.conftest as conftest
import logging
import filecmp
import pytest
import os

logging.getLogger("py4j").setLevel(logging.INFO)

import chemistry_and_robots.data_model as onto

pytest_plugins = ["docker_compose"]

# ----------------------------------------------------------------------------------
# Test cases for sparql_client 
# ----------------------------------------------------------------------------------
def test_amount_of_triples_none_zero(initialise_triples):
    sparql_client = initialise_triples
    assert sparql_client.getAmountOfTriples() != 0

def test_getDesignVariables(initialise_triples):
    sparql_client = initialise_triples
    design_var_list = sparql_client.getDesignVariables(TargetIRIs.DOE_DOMAIN_IRI.value)
    design_var_iri_list = [var.instance_iri for var in design_var_list]
    assert len(design_var_iri_list) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(iri in design_var_iri_list for iri in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in design_var_list)

@pytest.mark.parametrize(
    "sys_res_iri,maximise",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.DOE_SYS_RES_1_IRI.value, [True]),
        ([TargetIRIs.DOE_SYS_RES_1_IRI.value], [True]),
        (TargetIRIs.DOE_SYS_RES_2_IRI.value, [False]),
        ([TargetIRIs.DOE_SYS_RES_2_IRI.value], [False]),
        (TargetIRIs.DOE_SYS_RES_IRI_LIST.value, [True, False]),
    ],
)
def test_getSystemResponses(initialise_triples, sys_res_iri, maximise):
    sparql_client = initialise_triples
    sys_res_list = sparql_client.getSystemResponses(sys_res_iri)
    length = len(sys_res_iri) if isinstance(sys_res_iri, list) else 1
    assert length == len(sys_res_list)
    assert all(all([isinstance(r, onto.SystemResponse), r.refersTo is not None]) for r in sys_res_list)
    dct_m = {s.instance_iri:s.maximise for s in sys_res_list}
    if isinstance(sys_res_iri, list):
        assert all(m == dct_m.get(s) for s, m in zip(sys_res_iri, maximise))

def test_getTSEMOSettings(initialise_triples):
    sparql_client = initialise_triples
    tsemo = sparql_client.getTSEMOSettings(TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value)
    assert tsemo.instance_iri == TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value
    assert isinstance(tsemo, onto.TSEMO)
    assert tsemo.nRetries is not None
    assert tsemo.nSpectralPoints is not None
    assert tsemo.nGenerations is not None
    assert tsemo.populationSize is not None

def test_getDoEStrategy(initialise_triples):
    sparql_client = initialise_triples
    strategy = sparql_client.getDoEStrategy(TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value)
    assert strategy.instance_iri == TargetIRIs.DOE_TSEMO_STRATEGY_IRI.value
    assert isinstance(strategy, onto.TSEMO)
    assert strategy.nRetries is not None
    assert strategy.nSpectralPoints is not None
    assert strategy.nGenerations is not None
    assert strategy.populationSize is not None

def test_getDoEDomain(initialise_triples):
    sparql_client = initialise_triples
    doe_domain_instance = sparql_client.getDoEDomain(TargetIRIs.DOE_DOMAIN_IRI.value)
    assert doe_domain_instance.instance_iri == TargetIRIs.DOE_DOMAIN_IRI.value
    lst_var = [v.instance_iri for v in doe_domain_instance.hasDesignVariable]
    assert len(lst_var) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(var in lst_var for var in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in doe_domain_instance.hasDesignVariable)

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_condition_iri",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value),
    ],
)
def test_getExpReactionCondition(initialise_triples, rxnexp_iri, rxnexp_condition_iri):
    sparql_client = initialise_triples
    rxn_condition_list = sparql_client.getExpReactionCondition(rxnexp_iri)
    assert len(rxnexp_condition_iri) == len(rxn_condition_list)
    for con in rxn_condition_list:
        assert all([isinstance(con, onto.ReactionCondition), con.clz is not None, isinstance(con.objPropWithExp, list),
            con.hasValue.hasUnit is not None, con.hasValue.hasNumericalValue is not None, con.instance_iri in rxnexp_condition_iri])
        if con.clz == onto.ONTOREACTION_STOICHIOMETRYRATIO:
            assert con.indicatesMultiplicityOf is not None
        elif con.clz == onto.ONTOREACTION_REACTIONSCALE:
            assert con.indicateUsageOf is not None

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_pref_indicator_iri,rxn_type",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONVARIATION),
    ],
)
def test_getExpPerformanceIndicator(initialise_triples, rxnexp_iri, rxnexp_pref_indicator_iri, rxn_type):
    sparql_client = initialise_triples
    perf_ind_list = sparql_client.getExpPerformanceIndicator(rxnexp_iri)
    assert len(rxnexp_pref_indicator_iri) == len(perf_ind_list)
    for ind in perf_ind_list:
        assert all([isinstance(ind, onto.PerformanceIndicator), ind.clz is not None, isinstance(ind.objPropWithExp, list), ind.instance_iri in rxnexp_pref_indicator_iri])
    if rxn_type == onto.ONTOREACTION_REACTIONEXPERIMENT:
        for ind in perf_ind_list:
            assert all([ind.hasValue.hasUnit is not None, ind.hasValue.hasNumericalValue is not None])
    elif rxn_type == onto.ONTOREACTION_REACTIONVARIATION:
        for ind in perf_ind_list:
            assert all([ind.hasValue is None])
    else:
        assert False

@pytest.mark.parametrize(
    "rxnexp_iri,expected_rxn_type",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, onto.ONTOREACTION_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, onto.ONTOREACTION_REACTIONVARIATION),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, onto.ONTOREACTION_REACTIONVARIATION),
    ],
)
def test_get_rdf_type_of_rxn_exp(initialise_triples, rxnexp_iri, expected_rxn_type):
    sparql_client = initialise_triples
    rxn_type = sparql_client.get_rdf_type_of_rxn_exp(rxnexp_iri)
    assert rxn_type == expected_rxn_type

@pytest.mark.parametrize(
    "rxnexp_iris,rxn_type,rxnexp_condition,rxnexp_perfind,input_chem,output_chem,reactor_assigned,chem_rxn",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        # TODO (TargetIRIs.NEW_RXN_EXP_1_IRI.value, []),isVariationOf
        # TODO (TargetIRIs.NEW_RXN_EXP_2_IRI.value, []),isVariationOf
        # TODO (TargetIRIs.NEW_RXN_EXP_3_IRI.value, []),isVariationOf
    ],
)
def test_getReactionExperiment(initialise_triples, rxnexp_iris, rxn_type, rxnexp_condition, rxnexp_perfind, input_chem, output_chem, reactor_assigned, chem_rxn):
    sparql_client = initialise_triples
    rxn_exp_list = sparql_client.getReactionExperiment(rxnexp_iris)
    length = len(rxnexp_iris) if isinstance(rxnexp_iris, list) else 1
    assert len(rxn_exp_list) == length
    for exp in rxn_exp_list:
        assert exp.clz == rxn_type.get(exp.instance_iri)
        assert len(exp.hasReactionCondition) == len(rxnexp_condition.get(exp.instance_iri))
        assert len(exp.hasPerformanceIndicator) == len(rxnexp_perfind.get(exp.instance_iri))
        assert len(exp.hasInputChemical) == len(input_chem.get(exp.instance_iri))
        assert len(exp.hasOutputChemical) == len(output_chem.get(exp.instance_iri))
        assert all(con in [c.instance_iri for c in exp.hasReactionCondition] for con in rxnexp_condition.get(exp.instance_iri))
        assert all(pre in [pi.instance_iri for pi in exp.hasPerformanceIndicator] for pre in rxnexp_perfind.get(exp.instance_iri))
        assert all(inp in [ic.instance_iri for ic in exp.hasInputChemical] for inp in input_chem.get(exp.instance_iri))
        assert all(out in [oc.instance_iri for oc in exp.hasOutputChemical] for out in output_chem.get(exp.instance_iri))
        assert exp.isAssignedTo == reactor_assigned.get(exp.instance_iri)
        assert exp.isOccurenceOf.instance_iri == chem_rxn.get(exp.instance_iri)

def test_getNewExperimentFromDoE(initialise_triples):
    sparql_client = initialise_triples
    new_exp_iri = sparql_client.getNewExperimentFromDoE(TargetIRIs.DOE_IRI.value)
    assert new_exp_iri is None

def test_getDoEHistoricalData(initialise_triples):
    sparql_client = initialise_triples
    hist_data_instance = sparql_client.getDoEHistoricalData(TargetIRIs.DOE_HIST_DATA_IRI.value)
    assert isinstance(hist_data_instance, onto.HistoricalData)
    assert len(hist_data_instance.refersTo) == len(TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)
    assert all(iri in [h.instance_iri for h in hist_data_instance.refersTo] for iri in TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)

def test_get_doe_instance(initialise_triples):
    sparql_client = initialise_triples
    doe_instance = sparql_client.get_doe_instance(TargetIRIs.DOE_IRI.value)
    # Check Domain
    assert doe_instance.hasDomain.instance_iri == TargetIRIs.DOE_DOMAIN_IRI.value
    lst_var = [v.instance_iri for v in doe_instance.hasDomain.hasDesignVariable]
    assert len(lst_var) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(var in lst_var for var in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable), var.refersTo is not None, var.upperLimit > var.lowerLimit]) for var in doe_instance.hasDomain.hasDesignVariable)
    # Check SystemResponse
    assert all(all([isinstance(r, onto.SystemResponse), r.refersTo is not None]) for r in doe_instance.hasSystemResponse)
    dct_m = {s.instance_iri:s.maximise for s in doe_instance.hasSystemResponse}
    assert all([int(TargetIRIs.DOE_SYS_RES_MAXIMISE_DICT.value[s]) == int(dct_m.get(s)) for s in TargetIRIs.DOE_SYS_RES_MAXIMISE_DICT.value])
    # Check Strategy
    strategy = doe_instance.usesStrategy
    assert strategy.instance_iri == TargetIRIs.DOE_STRATEGY_IRI.value
    assert isinstance(strategy, onto.TSEMO)
    assert strategy.nRetries is not None
    assert strategy.nSpectralPoints is not None
    assert strategy.nGenerations is not None
    assert strategy.populationSize is not None

@pytest.mark.parametrize(
    "rxn_variation_iri,expected_rxn_rxp_iri",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value),
    ],
)
def test_get_rxn_exp_iri_given_rxn_variation(initialise_triples, rxn_variation_iri, expected_rxn_rxp_iri):
    sparql_client = initialise_triples
    rxn_exp_iri = sparql_client.get_rxn_exp_iri_given_rxn_variation(rxn_variation_iri)
    assert rxn_exp_iri == expected_rxn_rxp_iri

def test_get_all_autosampler_with_fill(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_all_autosampler_with_fill()
    assert len(response) == 1
    autosampler = response[0]
    assert isinstance(autosampler, onto.AutoSampler)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value

def test_get_r4_reactor_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r4_reactor_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R4REACTORS.value)
    list_reactor_iri = [res.instance_iri for res in response]
    assert len(set(list_reactor_iri).difference(set(TargetIRIs.LIST_DUMMY_R4REACTORS.value))) == 0

def test_get_r2_pump_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    response = sparql_client.get_r2_pump_given_vapourtec_rs400(TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert len(response) == len(TargetIRIs.LIST_DUMMY_R2PUMPS.value)
    list_pumps_iri = [res.instance_iri for res in response]
    assert len(set(list_pumps_iri).difference(set(TargetIRIs.LIST_DUMMY_R2PUMPS.value))) == 0

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_conducted",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP.value),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
#     ],
# )
# def test_get_rxn_exp_conducted_in_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_conducted):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_conducted_in_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_conducted)
#     assert len(set(response).difference(set(rxn_exp_conducted))) == 0

@pytest.mark.parametrize(
    "r4_reactor_iri,rxn_exp_assigned",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY.value),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY.value),
    ],
)
def test_get_rxn_exp_assigned_to_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_assigned):
    sparql_client = initialise_triples
    response = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert len(response) == len(rxn_exp_assigned)
    assert len(set(response).difference(set(rxn_exp_assigned))) == 0

# TODO commented out for now, decide whether to keep it before merging back to develop based on the function in sparql_client.py
# @pytest.mark.parametrize(
#     "r4_reactor_iri,rxn_exp_pending",
#     [
#         (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, []),
#         (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, []),
#     ],
# )
# def test_get_rxn_exp_pending_for_r4_reactor(initialise_triples, r4_reactor_iri, rxn_exp_pending):
#     sparql_client = initialise_triples
#     response = sparql_client.get_rxn_exp_pending_for_r4_reactor(r4_reactor_iri)
#     assert len(response) == len(rxn_exp_pending)
#     assert len(set(response).difference(set(rxn_exp_pending))) == 0

@pytest.mark.parametrize(
    "rxnexp_iri,input_chemical_iri",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI.value),
    ],
)
def test_get_input_chemical_of_rxn_exp(initialise_triples, rxnexp_iri, input_chemical_iri):
    sparql_client = initialise_triples
    response = sparql_client.get_input_chemical_of_rxn_exp(rxnexp_iri)
    list_input_chemical = [res.instance_iri for res in response]
    assert len(list_input_chemical) == len(input_chemical_iri)
    assert len(set(list_input_chemical).difference(set(input_chemical_iri))) == 0

def test_get_vapourtec_rs400_given_autosampler(initialise_triples):
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value
    response = sparql_client.get_vapourtec_rs400_given_autosampler(autosampler)
    assert  response.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value

@pytest.mark.parametrize(
    "new_rxn_exp_iri,list_r4_reactor_iri,vapourtec_rs400,agilent_hplc",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.LIST_DUMMY_R4REACTORS.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
    ],
)
def test_get_preferred_vapourtec_rs400(initialise_triples, new_rxn_exp_iri, list_r4_reactor_iri, vapourtec_rs400, agilent_hplc):
    sparql_client = initialise_triples
    response = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(response) == 1
    assert response[0].instance_iri == new_rxn_exp_iri
    preferred_rs400, preferred_r4_reactor, associated_agilent_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Should return None as none of the digital twin is managed by agent
    assert None == preferred_rs400
    assert None == preferred_r4_reactor
    assert None == associated_agilent_hplc

    # Add agent to manage the digital twin
    temp_agent_1 = "http://"+str(uuid.uuid4())
    temp_agent_2 = "http://"+str(uuid.uuid4())
    sparql_client.register_agent_with_hardware(temp_agent_1, vapourtec_rs400)
    sparql_client.register_agent_with_hardware(temp_agent_2, agilent_hplc)
    # Query again, should return digital twin now
    preferred_rs400, preferred_r4_reactor, associated_agilent_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    assert preferred_r4_reactor.instance_iri in list_r4_reactor_iri
    assert preferred_rs400.instance_iri == vapourtec_rs400
    assert associated_agilent_hplc.instance_iri == agilent_hplc

    # Change the status to Null
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_NULL, 0)
    # Now perform the same checking
    new_rs400, new_r4_reactor, new_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Change back the status to Idle
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_IDLE, 0)
    # Now perform the same checking, the returned values should be None, None
    assert None == new_rs400
    assert None == new_r4_reactor
    assert None == new_hplc

    # Remove temp_agent that manages the digital twin
    sparql_client.performUpdate("""DELETE WHERE {<%s> <%s> ?temp_agent_1. <%s> <%s> ?temp_agent_2.}""" % (
        vapourtec_rs400, onto.ONTOLAB_ISMANAGEDBY, agilent_hplc, onto.ONTOLAB_ISMANAGEDBY
    ))

@pytest.mark.parametrize(
    "new_rxn_exp_iri,r4_reactor_iri",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value),
    ],
)
def test_assign_and_remove_rxn_exp_to_r4_reactor(initialise_triples, new_rxn_exp_iri, r4_reactor_iri):
    sparql_client = initialise_triples
    response1 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response1

    sparql_client.assign_rxn_exp_to_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response2 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri in response2

    sparql_client.remove_rxn_exp_from_r4_reactor(new_rxn_exp_iri, r4_reactor_iri)
    response3 = sparql_client.get_rxn_exp_assigned_to_r4_reactor(r4_reactor_iri)
    assert new_rxn_exp_iri not in response3

@pytest.mark.parametrize(
    "rxn_exp_iri,prior_rxn_exp",
    [
        (TargetIRIs.RXN_EXP_QUEUE_1.value, TargetIRIs.RXN_EXP_1_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_2.value, TargetIRIs.RXN_EXP_2_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_3.value, TargetIRIs.RXN_EXP_3_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_4.value, TargetIRIs.RXN_EXP_4_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_5.value, TargetIRIs.RXN_EXP_5_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_6.value, TargetIRIs.RXN_EXP_6_PRIOR.value),
        (TargetIRIs.RXN_EXP_QUEUE_7.value, TargetIRIs.RXN_EXP_7_PRIOR.value),
    ],
)
def test_get_prior_rxn_exp_in_queue(initialise_triples, rxn_exp_iri, prior_rxn_exp):
    sparql_client = initialise_triples
    rxn_exp_queue = sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_iri)
    assert all(item in [*rxn_exp_queue] for item in prior_rxn_exp)

@pytest.mark.parametrize(
    "rxn_exp_iri,rxn_type,chem_rxn_iri,reactant,product,catalyst,solvent",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, onto.ONTOREACTION_REACTIONEXPERIMENT, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, onto.ONTOREACTION_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, onto.ONTOREACTION_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, onto.ONTOREACTION_REACTIONVARIATION, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value),
    ],
)
def test_get_chemical_reaction(initialise_triples, rxn_exp_iri, rxn_type, chem_rxn_iri, reactant, product, catalyst, solvent):
    sparql_client = initialise_triples
    if rxn_type == onto.ONTOREACTION_REACTIONEXPERIMENT:
        chem_rxn = sparql_client.get_chemical_reaction(rxn_exp_iri)
    elif rxn_type == onto.ONTOREACTION_REACTIONVARIATION:
        chem_rxn = sparql_client.get_chemical_reaction_of_rxn_variation(rxn_exp_iri)
    else:
        assert False
    assert chem_rxn.instance_iri == chem_rxn_iri
    dict_reactant = {reactant.instance_iri:reactant.hasUniqueSpecies for reactant in chem_rxn.hasReactant}
    assert dict_reactant == reactant
    dict_product = {product.instance_iri:product.hasUniqueSpecies for product in chem_rxn.hasProduct}
    assert dict_product == product
    dict_catalyst = {catalyst.instance_iri:catalyst.hasUniqueSpecies for catalyst in chem_rxn.hasCatalyst}
    assert dict_catalyst == catalyst
    dict_solvent = {solvent.instance_iri:solvent.hasUniqueSpecies for solvent in chem_rxn.hasSolvent}
    assert dict_solvent == solvent

def test_get_internal_standard(initialise_triples):
    sparql_client = initialise_triples
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert internal_standard.instance_iri == TargetIRIs.PHASECOMPONENT_INTERNAL_STANDARD_IRI.value
    assert internal_standard.representsOccurenceOf == TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value
    assert internal_standard.hasProperty.hasValue.numericalValue == TargetIRIs.MOLARITY_INTERNAL_STANDARD.value

def test_get_hplc_method(initialise_triples):
    sparql_client = initialise_triples
    hplc_method = sparql_client.get_hplc_method(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert hplc_method.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_method.usesInternalStandard == internal_standard
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_hplc_method_given_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    hplc_method = sparql_client.get_hplc_method_given_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert hplc_method.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_method.usesInternalStandard == internal_standard
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_chromatogram_point_of_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    list_chrom_pts = sparql_client.get_chromatogram_point_of_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert len(list_chrom_pts) == len(TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value)
    for pt in list_chrom_pts:
        assert pt.instance_iri in TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value
        assert pt.indicatesComponent.instance_iri is not None
        assert pt.indicatesComponent.representsOccurenceOf is not None
        assert isinstance(pt.indicatesComponent.hasProperty, onto.OntoCAPE_PhaseComponentConcentration)
        assert pt.indicatesComponent.hasProperty.hasValue.numericalValue is not None
        assert pt.atRetentionTime.hasValue.hasNumericalValue > 0
        assert pt.atRetentionTime.hasValue.hasUnit is not None
        assert pt.hasPeakArea.hasValue.hasNumericalValue > 0
        assert pt.hasPeakArea.hasValue.hasUnit is not None

def test_get_existing_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    list_chrom_pts = sparql_client.get_chromatogram_point_of_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert len(list_chrom_pts) == len(TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value)
    assert all(pt.instance_iri in TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value for pt in list_chrom_pts)
    hplc_report = sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert all(pt in list_chrom_pts for pt in hplc_report.records)
    assert hplc_report.generatedFor.instance_iri == TargetIRIs.CHEMICAL_SOLUTION_FOR_OUTPUTCHEMICAL_4_IRI.value
    assert hplc_report.remoteFilePath is not None
    assert hplc_report.localFilePath is not None
    assert hplc_report.lastLocalModifiedAt > 0
    assert hplc_report.lastUploadedAt > 0

def test_get_hplc_job_given_hplc_report_instance(initialise_triples):
    sparql_client = initialise_triples
    hplc_report_instance = sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    hplc_job_instance = sparql_client.get_hplc_job_given_hplc_report_instance(hplc_report_instance)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport == hplc_report_instance

def test_get_hplc_job_given_hplc_report_iri(initialise_triples):
    sparql_client = initialise_triples
    hplc_job_instance = sparql_client.get_hplc_job_given_hplc_report_iri(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport == sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)

def test_get_hplc_job(initialise_triples):
    sparql_client = initialise_triples
    hplc_job_instance = sparql_client.get_hplc_job(TargetIRIs.HPLCJOB_DUMMY_IRI.value)
    assert hplc_job_instance.instance_iri == TargetIRIs.HPLCJOB_DUMMY_IRI.value
    assert hplc_job_instance.characterises.instance_iri == TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value
    assert hplc_job_instance.usesMethod.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_job_instance.hasReport.instance_iri == TargetIRIs.HPLCREPORT_DUMMY_IRI.value

@pytest.mark.parametrize(
    "hplc_digital_twin,hplc_remote_file_path,expected_rxn_exp",
    [
        (TargetIRIs.HPLC_1_POST_PROC_IRI.value, 'placeholder_1', TargetIRIs.NEW_RXN_EXP_1_IRI.value),
        (TargetIRIs.HPLC_2_POST_PROC_IRI.value, 'placeholder_2', TargetIRIs.NEW_RXN_EXP_2_IRI.value),
    ],
)
def test_identify_rxn_exp_when_uploading_hplc_report(initialise_triples, hplc_digital_twin, hplc_remote_file_path, expected_rxn_exp):
    sparql_client = initialise_triples
    rxn_exp = sparql_client.identify_rxn_exp_when_uploading_hplc_report(hplc_digital_twin, hplc_remote_file_path)
    assert rxn_exp == expected_rxn_exp

@pytest.mark.parametrize(
    "hplc_iri,expected_local_folder_path,expected_file_extension",
    [
        (TargetIRIs.HPLC_DUMMY_IRI.value, TargetIRIs.HPLC_LOCAL_FOLDER_PATH.value, onto.XLSFILE_EXTENSION),
        (TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.HPLC_LOCAL_FOLDER_PATH.value, onto.XLSFILE_EXTENSION),
        (TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.HPLC_LOCAL_FOLDER_PATH.value, onto.TXTFILE_EXTENSION),
    ],
)
def test_get_hplc_local_report_folder_path_n_file_extension(initialise_triples, hplc_iri, expected_local_folder_path, expected_file_extension):
    sparql_client = initialise_triples
    local_folder_path, file_extension = sparql_client.get_hplc_local_report_folder_path_n_file_extension(hplc_iri)
    assert (local_folder_path, file_extension) == (expected_local_folder_path, expected_file_extension)

@pytest.mark.parametrize(
    "local_file_path,hplc_digital_twin,chemical_solution_iri,internal_standard_species,internal_standard_run_conc",
    [
        (conftest.HPLC_XLS_REPORT_FILE, TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_SOLUTION_1_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value),
        (conftest.HPLC_TXT_REPORT_FILE, TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_SOLUTION_2_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value),
    ],
)
def test_upload_download_process_raw_hplc_report(initialise_triples, generate_random_download_path, local_file_path, hplc_digital_twin, chemical_solution_iri, internal_standard_species, internal_standard_run_conc):
    """This is an integration test of five methods that are called by different agents: upload_raw_hplc_report_to_fs_kg, get_raw_hplc_report_remote_path_and_extension,
    download_remote_raw_hplc_report, connect_hplc_report_with_chemical_solution, and process_raw_hplc_report."""
    sparql_client = initialise_triples
    timestamp_last_modified = os.path.getmtime(local_file_path)

    # First upload and download the report (as part of HPLCInput Agent), make sure the content is the same
    hplc_report_iri = sparql_client.upload_raw_hplc_report_to_fs_kg(local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified, hplc_digital_twin=hplc_digital_twin)
    remote_file_path, file_extension = sparql_client.get_raw_hplc_report_remote_path_and_extension(hplc_report_iri)
    full_downloaded_path = generate_random_download_path(file_extension)
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Second make the connection between HPLCReport and ChemicalSolution (as part of Execution Agent)
    sparql_client.connect_hplc_report_with_chemical_solution(hplc_report_iri, chemical_solution_iri)

    # Third process the raw report (as part of PostProc Agent)
    hplc_report_instance = sparql_client.process_raw_hplc_report(hplc_report_iri, internal_standard_species, internal_standard_run_conc)
    assert hplc_report_instance.instance_iri == hplc_report_iri
    assert hplc_report_instance.remoteFilePath == remote_file_path
    assert hplc_report_instance.localFilePath == local_file_path
    assert (hplc_report_instance.lastLocalModifiedAt - timestamp_last_modified) <= 0.00001
    assert hplc_report_instance.lastUploadedAt > hplc_report_instance.lastLocalModifiedAt
    list_chrom_pts = hplc_report_instance.records
    for pt in list_chrom_pts:
        assert pt.instance_iri.startswith(pt.namespace_for_init)
        assert pt.indicatesComponent.instance_iri is not None
        assert pt.indicatesComponent.representsOccurenceOf is not None
        assert isinstance(pt.indicatesComponent.hasProperty, onto.OntoCAPE_PhaseComponentConcentration)
        assert pt.indicatesComponent.hasProperty.hasValue.numericalValue is not None
        assert pt.atRetentionTime.hasValue.hasNumericalValue > 0
        assert pt.atRetentionTime.hasValue.hasUnit is not None
        assert pt.hasPeakArea.hasValue.hasNumericalValue > 0
        assert pt.hasPeakArea.hasValue.hasUnit is not None
    dct_phase_comp = {pt.indicatesComponent.instance_iri:pt.indicatesComponent for pt in list_chrom_pts}
    chemical_solution_instance = hplc_report_instance.generatedFor
    assert chemical_solution_instance.instance_iri == chemical_solution_iri
    dct_phase_comp_chemical_solution = {pc.instance_iri:pc for pc in chemical_solution_instance.refersToMaterial.thermodynamicBehaviour.isComposedOfSubsystem}
    assert len(dct_phase_comp) == len(dct_phase_comp_chemical_solution)
    assert all([dct_phase_comp[pc] == dct_phase_comp_chemical_solution[pc] for pc in dct_phase_comp])
    dct_conc_phase_comp = {pc.hasProperty.instance_iri:pc.hasProperty for pc in chemical_solution_instance.refersToMaterial.thermodynamicBehaviour.isComposedOfSubsystem}
    dct_conc_composition = {conc.instance_iri:conc for conc in chemical_solution_instance.refersToMaterial.thermodynamicBehaviour.has_composition.comprisesDirectly}
    assert len(dct_conc_phase_comp) == len(dct_conc_composition)
    assert all([dct_conc_phase_comp[conc] == dct_conc_composition[conc] for conc in dct_conc_phase_comp])

@pytest.mark.parametrize(
    "reactor_iri,reactor_volume,reactor_volume_unit",
    [
        (TargetIRIs.VAPOURTECR4REACTOR_DUMMY_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_DUMMY_VOLUME.value, onto.OM_MILLILITRE),
        (TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI.value, TargetIRIs.VAPOURTECR4REACTOR_ANOTHER_DUMMY_VOLUME.value, onto.OM_MILLILITRE),
    ],
)
def test_get_reactor_volume_given_reactor(initialise_triples, reactor_iri, reactor_volume, reactor_volume_unit):
    sparql_client = initialise_triples
    assert sparql_client.get_reactor_volume_given_reactor(reactor_iri) == (reactor_volume, reactor_volume_unit)

def test_get_rxn_exp_associated_with_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    rxn_exp = sparql_client.get_rxn_exp_associated_with_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    expected_rxn_exp = sparql_client.getReactionExperiment(TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value)[0]
    assert rxn_exp == expected_rxn_exp

def test_get_internal_standard_associated_with_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    internal_standard = sparql_client.get_internal_standard_associated_with_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    expected_internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert internal_standard == expected_internal_standard

@pytest.mark.parametrize(
    "hplc_digital_twin,hplc_local_file,expected_remote_path",
    [
        (TargetIRIs.HPLC_DUMMY_IRI.value, TargetIRIs.HPLCREPORT_DUMMY_LOCAL_PATH.value, TargetIRIs.HPLCREPORT_DUMMY_REMOTE_PATH.value),
    ],
)
def test_get_remote_hplc_report_path_given_local_file(initialise_triples, hplc_digital_twin, hplc_local_file, expected_remote_path):
    sparql_client = initialise_triples
    remote_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, hplc_local_file)
    assert remote_path == expected_remote_path

#############################################
## sparql_client.py functions to be tested ##
#############################################
# def test_(initialise_triples):
#     sparql_client = initialise_triples
#     sparql_client = ChemistryAndRobotsSparqlClient()
#     pass

# get_output_chemical_of_rxn_exp
# get_ontocape_material
# get_autosampler_from_vapourtec_rs400
# sort_r2_pumps_in_vapourtec_rs400
# get_rxn_con_or_perf_ind

# get_r4_reactor_rxn_exp_assigned_to

# updateNewExperimentInKG
# create_equip_settings_for_rs400_from_rxn_exp
# get_autosampler_site_given_input_chemical
# write_equip_settings_to_kg
# write_performance_indicator_back_to_kg
# write_output_chemical_of_chem_sol_back_to_kg

# get_species_molar_mass_kilogrampermole
# get_matching_species_from_hplc_results

# get_species_density
# get_species_material_cost
# get_species_eco_score
# identify_hplc_method_when_uploading_hplc_report
