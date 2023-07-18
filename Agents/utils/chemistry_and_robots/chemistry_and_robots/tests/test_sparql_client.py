from datetime import datetime
import random
import time
import uuid

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient
from chemistry_and_robots.tests.conftest import TargetIRIs
import chemistry_and_robots.tests.conftest as conftest
from rdflib import Literal
from rdflib import URIRef
from rdflib import Graph
import filecmp
import pytest
import os

import chemistry_and_robots.data_model as onto
import chemistry_and_robots.kg_operations.dict_and_list as dal
import chemistry_and_robots.hardware.hplc as hplc

pytest_plugins = ["docker_compose"]

# ----------------------------------------------------------------------------------
# Test cases for sparql_client
# ----------------------------------------------------------------------------------
# collect_triples_for_equip_settings and collect_triples_for_new_experiment are
# tested in integration test of agents

def test_get_ontology_tbox_version(initialise_triples):
    sparql_client = initialise_triples
    assert sparql_client.get_ontology_tbox_version(onto.ONTODOE) is not None
    with pytest.raises(Exception):
        sparql_client.get_ontology_tbox_version(f'http://fake_ontology/{str(uuid.uuid4())}/')

def test_amount_of_triples_none_zero(initialise_triples):
    sparql_client = initialise_triples
    assert sparql_client.getAmountOfTriples() != 0

def test_getDesignVariables(initialise_triples):
    sparql_client = initialise_triples
    design_var_list = sparql_client.getDesignVariables(TargetIRIs.DOE_DOMAIN_IRI.value)
    design_var_iri_list = [var.instance_iri for var in design_var_list]
    assert len(design_var_iri_list) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(iri in design_var_iri_list for iri in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable),
                    var.refersToQuantity is not None,
                    var.refersToQuantity.hasUnit is not None,
                    var.upperLimit > var.lowerLimit]) for var in design_var_list)

def test_get_fixed_parameters(initialise_triples):
    sparql_client = initialise_triples
    param_list = sparql_client.get_fixed_parameters(TargetIRIs.DOE_NO_PRIOR_DATA_DOMAIN_IRI.value)
    param_iri_list = [param.instance_iri for param in param_list]
    assert dal.check_if_two_lists_equal(
        param_iri_list,
        TargetIRIs.DOE_NO_PRIOR_DATA_FIXED_PARAM_IRI_LIST.value
    )
    assert all(
        all(
            [
                isinstance(param, onto.FixedParameter),
                param.refersToQuantity is not None,
                param.refersToQuantity.hasValue is not None,
                param.refersToQuantity.hasValue.hasUnit is not None,
                param.refersToQuantity.hasValue.hasNumericalValue is not None
            ]
        ) for param in param_list
    )

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
    assert all(all([isinstance(r, onto.SystemResponse), r.refersToQuantity is not None]) for r in sys_res_list)
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
    assert all(all([isinstance(var, onto.ContinuousVariable),
                    var.refersToQuantity is not None,
                    var.refersToQuantity.hasUnit is not None,
                    var.upperLimit > var.lowerLimit]) for var in doe_domain_instance.hasDesignVariable)

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_condition_iri",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value),
        (
            [
                TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value,
                TargetIRIs.NEW_RXN_EXP_2_IRI.value,
                TargetIRIs.NEW_RXN_EXP_3_IRI.value,
            ],
            [
                TargetIRIs.EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST.value,
                TargetIRIs.NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value,
                TargetIRIs.NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value,
            ]),
    ],
)
def test_getExpReactionCondition(initialise_triples, rxnexp_iri, rxnexp_condition_iri):
    sparql_client = initialise_triples
    if isinstance(rxnexp_iri, str):
        rxnexp_iri = [rxnexp_iri]
        rxnexp_condition_iri = [rxnexp_condition_iri]
    dict_rxn_condition = sparql_client.getExpReactionCondition(rxnexp_iri)
    for i in range(len(rxnexp_iri)):
        rxn_condition_list = dict_rxn_condition[rxnexp_iri[i]]
        assert len(rxnexp_condition_iri[i]) == len(rxn_condition_list)
        for con in rxn_condition_list:
            assert all([isinstance(con, onto.ReactionCondition), con.clz is not None, isinstance(con.objPropWithExp, list),
                con.hasValue.hasUnit is not None, con.hasValue.hasNumericalValue is not None, con.instance_iri in rxnexp_condition_iri[i]])
            if con.clz == onto.ONTOREACTION_STOICHIOMETRYRATIO:
                assert con.indicatesMultiplicityOf is not None
            elif con.clz == onto.ONTOREACTION_REACTIONSCALE:
                assert con.indicatesUsageOf is not None

@pytest.mark.parametrize(
    "rxnexp_iri,rxnexp_pref_indicator_iri,rxn_type",
    [# here we are testing that the function should work wether the passed in iri is already a list or not
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONEXPERIMENT),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value, onto.ONTOREACTION_REACTIONVARIATION),
        (
            [
                TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value,
                TargetIRIs.NEW_RXN_EXP_2_IRI.value,
                TargetIRIs.NEW_RXN_EXP_3_IRI.value,
            ],
            [
                TargetIRIs.EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST.value,
                TargetIRIs.EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST.value,
                TargetIRIs.NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value,
                TargetIRIs.NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value,
            ],
            [
                onto.ONTOREACTION_REACTIONEXPERIMENT,
                onto.ONTOREACTION_REACTIONEXPERIMENT,
                onto.ONTOREACTION_REACTIONEXPERIMENT,
                onto.ONTOREACTION_REACTIONEXPERIMENT,
                onto.ONTOREACTION_REACTIONVARIATION,
                onto.ONTOREACTION_REACTIONVARIATION,
            ]
        ),
    ],
)
def test_getExpPerformanceIndicator(initialise_triples, rxnexp_iri, rxnexp_pref_indicator_iri, rxn_type):
    sparql_client = initialise_triples
    if isinstance(rxnexp_iri, str):
        rxnexp_iri = [rxnexp_iri]
        rxnexp_pref_indicator_iri = [rxnexp_pref_indicator_iri]
        rxn_type = [rxn_type]
    dict_perf_ind = sparql_client.getExpPerformanceIndicator(rxnexp_iri)
    for i, rxn in enumerate(rxnexp_iri):
        if rxnexp_pref_indicator_iri[i] is not None:
            perf_ind_list = dict_perf_ind[rxn]
            assert len(rxnexp_pref_indicator_iri[i]) == len(perf_ind_list)
            for ind in perf_ind_list:
                assert all([isinstance(ind, onto.PerformanceIndicator), ind.clz is not None, isinstance(ind.objPropWithExp, list), ind.instance_iri in rxnexp_pref_indicator_iri[i]])
            if rxn_type[i] == onto.ONTOREACTION_REACTIONEXPERIMENT:
                for ind in perf_ind_list:
                    assert all([ind.hasValue.hasUnit is not None, ind.hasValue.hasNumericalValue is not None])
            elif rxn_type[i] == onto.ONTOREACTION_REACTIONVARIATION:
                for ind in perf_ind_list:
                    assert all([ind.hasValue is None])
            else:
                assert False
        else:
            assert dict_perf_ind[rxn] is None

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
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.NEW_RXN_EXP_1_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.NEW_RXN_EXP_2_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        ([TargetIRIs.NEW_RXN_EXP_3_IRI.value], TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
        (TargetIRIs.LIST_NEW_RXN_EXP.value, TargetIRIs.RXNEXP_TYPE_DICT.value, TargetIRIs.RXNEXP_REACTION_CONDITION_DICT.value,
            TargetIRIs.RXNEXP_PERFORMANCE_INDICATOR_DICT.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value,
            TargetIRIs.RXNEXP_REACTOR_ASSIGNED_DICT.value, TargetIRIs.RXNEXP_CHEMICAL_REACTION_IRI_DICT.value),
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
        if exp.hasPerformanceIndicator is not None:
            assert len(exp.hasPerformanceIndicator) == len(rxnexp_perfind.get(exp.instance_iri))
            assert all(pre in [pi.instance_iri for pi in exp.hasPerformanceIndicator] for pre in rxnexp_perfind.get(exp.instance_iri))
        else:
            assert rxnexp_perfind.get(exp.instance_iri) is None
        assert len(exp.hasInputChemical) == len(input_chem.get(exp.instance_iri))
        if exp.hasOutputChemical is not None:
            assert len(exp.hasOutputChemical) == len(output_chem.get(exp.instance_iri))
            assert all(out in [oc.instance_iri for oc in exp.hasOutputChemical] for out in output_chem.get(exp.instance_iri))
        else:
            assert output_chem.get(exp.instance_iri) is None
        assert all(con in [c.instance_iri for c in exp.hasReactionCondition] for con in rxnexp_condition.get(exp.instance_iri))
        assert all(inp in [ic.instance_iri for ic in exp.hasInputChemical] for inp in input_chem.get(exp.instance_iri))
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
    assert len(hist_data_instance.refersToExperiment) == len(TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)
    assert all(iri in [h.instance_iri for h in hist_data_instance.refersToExperiment] for iri in TargetIRIs.DOE_HIST_DATE_REFERTO_IRI.value)

def test_get_doe_instance(initialise_triples):
    sparql_client = initialise_triples
    doe_instance = sparql_client.get_doe_instance(TargetIRIs.DOE_IRI.value)
    # Check Domain
    assert doe_instance.hasDomain.instance_iri == TargetIRIs.DOE_DOMAIN_IRI.value
    lst_var = [v.instance_iri for v in doe_instance.hasDomain.hasDesignVariable]
    assert len(lst_var) == len(TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(var in lst_var for var in TargetIRIs.DOE_CONT_VAR_IRI_LIST.value)
    assert all(all([isinstance(var, onto.ContinuousVariable),
                    var.refersToQuantity is not None,
                    var.refersToQuantity.hasUnit is not None,
                    var.upperLimit > var.lowerLimit]) for var in doe_instance.hasDomain.hasDesignVariable)
    # Check SystemResponse
    assert all(all([isinstance(r, onto.SystemResponse), r.refersToQuantity is not None]) for r in doe_instance.hasSystemResponse)
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

@pytest.mark.parametrize(
    "rxnexp_iris,rxn_input_chemical_dict",
    [
        (TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.RXNEXP_INPUT_CHEMICAL_DICT.value),
    ],
)
def test_get_input_chemical_of_rxn_exp(initialise_triples, rxnexp_iris, rxn_input_chemical_dict):
    """Also tests get_chemical."""
    sparql_client = initialise_triples
    dict_queried_input_chemical = sparql_client.get_input_chemical_of_rxn_exp(rxnexp_iris)
    for rxnexp_iri in rxnexp_iris:
        list_input_chemical = [in_chem.instance_iri for in_chem in dict_queried_input_chemical[rxnexp_iri]]
        assert len(list_input_chemical) == len(rxn_input_chemical_dict[rxnexp_iri])
        assert len(set(list_input_chemical).difference(set(rxn_input_chemical_dict[rxnexp_iri]))) == 0

def test_get_vapourtec_rs400_given_autosampler(initialise_triples):
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value
    response = sparql_client.get_vapourtec_rs400_given_autosampler(autosampler)
    assert  response.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value

@pytest.mark.parametrize(
    "new_rxn_exp_iri,vapourtec_rs400,agilent_hplc",
    [
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, TargetIRIs.HPLC_DUMMY_IRI.value),
    ],
)
def test_get_preferred_vapourtec_rs400(initialise_triples, new_rxn_exp_iri, vapourtec_rs400, agilent_hplc):
    sparql_client = initialise_triples
    response = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(response) == 1
    assert response[0].instance_iri == new_rxn_exp_iri
    preferred_rs400, associated_agilent_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Should return None as none of the digital twin is managed by agent
    assert None == preferred_rs400
    assert None == associated_agilent_hplc

    # Add agent to manage the digital twin
    temp_agent_1 = "http://"+str(uuid.uuid4())
    temp_agent_2 = "http://"+str(uuid.uuid4())
    sparql_client.register_agent_with_hardware(temp_agent_1, vapourtec_rs400)
    sparql_client.register_agent_with_hardware(temp_agent_2, agilent_hplc)
    # Query again, should return digital twin now
    preferred_rs400, associated_agilent_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    assert preferred_rs400.instance_iri == vapourtec_rs400
    assert associated_agilent_hplc.instance_iri == agilent_hplc

    # Change the status to Null
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_NULL, 0)
    # Now query again, should return None now
    new_rs400, new_hplc = sparql_client.get_preferred_vapourtec_rs400(response[0])
    # Now perform the same checking, the returned values should be None, None
    assert None == new_rs400
    assert None == new_hplc

    # Change back the status to Idle
    sparql_client.update_vapourtec_rs400_state(vapourtec_rs400, onto.ONTOVAPOURTEC_IDLE, 0)
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
    "rxn_exp_iri,prior_rxn_exp,vapourtec_execution_agent_iri",
    [
        (TargetIRIs.RXN_EXP_QUEUE_1.value, TargetIRIs.RXN_EXP_1_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_2.value, TargetIRIs.RXN_EXP_2_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_3.value, TargetIRIs.RXN_EXP_3_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_4.value, TargetIRIs.RXN_EXP_4_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_5.value, TargetIRIs.RXN_EXP_5_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_6.value, TargetIRIs.RXN_EXP_6_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
        (TargetIRIs.RXN_EXP_QUEUE_7.value, TargetIRIs.RXN_EXP_7_PRIOR.value, TargetIRIs.DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI.value),
    ],
)
def test_get_prior_rxn_exp_in_queue(initialise_triples, rxn_exp_iri, prior_rxn_exp, vapourtec_execution_agent_iri):
    sparql_client = initialise_triples
    rxn_exp_queue = sparql_client.get_prior_rxn_exp_in_queue(rxn_exp_iri, vapourtec_execution_agent_iri)
    assert all(item in [*rxn_exp_queue] for item in prior_rxn_exp)

@pytest.mark.parametrize(
    "rxn_exp_iri,chem_rxn_iri,reactant,product,catalyst,solvent,doe_template_iri",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
        TargetIRIs.DOE_TEMPLATE_IRI.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
        TargetIRIs.DOE_TEMPLATE_IRI.value),
        (
            [
                TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value,
                TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value,
                TargetIRIs.NEW_RXN_EXP_2_IRI.value,
                TargetIRIs.NEW_RXN_EXP_3_IRI.value,
            ],
            [
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
                TargetIRIs.CHEMICAL_REACTION_IRI.value,
            ],
            [
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
                TargetIRIs.REACTANT_SPECIES_DICTIONARY.value,
            ],
            [
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
                TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
            ],
            [
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
                TargetIRIs.CATALYST_SPECIES_DICTIONARY.value,
            ],
            [
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
                TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
            ],
            [
                TargetIRIs.DOE_TEMPLATE_IRI.value,
                TargetIRIs.DOE_TEMPLATE_IRI.value,
                TargetIRIs.DOE_TEMPLATE_IRI.value,
                TargetIRIs.DOE_TEMPLATE_IRI.value,
                TargetIRIs.DOE_TEMPLATE_IRI.value,
                TargetIRIs.DOE_TEMPLATE_IRI.value,
            ]
        ),
    ],
)
def test_get_chemical_reaction(initialise_triples, rxn_exp_iri, chem_rxn_iri, reactant, product, catalyst, solvent, doe_template_iri):
    """NOTE get_ontokin_species_from_chem_rxn is tested as part of testing get_chemical_reaction."""
    sparql_client = initialise_triples
    if not isinstance(rxn_exp_iri, list):
        rxn_exp_iri = [rxn_exp_iri]
        chem_rxn_iri = [chem_rxn_iri]
        reactant = [reactant]
        product = [product]
        catalyst = [catalyst]
        solvent = [solvent]
        doe_template_iri = [doe_template_iri]
    dict_chem_rxn = sparql_client.get_chemical_reaction(rxn_exp_iri)
    for i in range(len(rxn_exp_iri)):
        chem_rxn = dict_chem_rxn[rxn_exp_iri[i]]
        assert chem_rxn.instance_iri == chem_rxn_iri[i]
        dict_reactant = {reactant.instance_iri:reactant.hasUniqueSpecies for reactant in chem_rxn.hasReactant}
        assert dict_reactant == reactant[i]
        dict_product = {product.instance_iri:product.hasUniqueSpecies for product in chem_rxn.hasProduct}
        assert dict_product == product[i]
        dict_catalyst = {catalyst.instance_iri:catalyst.hasUniqueSpecies for catalyst in chem_rxn.hasCatalyst}
        assert dict_catalyst == catalyst[i]
        dict_solvent = {solvent.instance_iri:solvent.hasUniqueSpecies for solvent in chem_rxn.hasSolvent}
        assert dict_solvent == solvent[i]
        assert chem_rxn.hasDoETemplate == doe_template_iri[i]

@pytest.mark.parametrize(
    "chem_rxn_iri,reactant,product,catalyst,solvent,doe_template_iri",
    [
        (TargetIRIs.CHEMICAL_REACTION_IRI.value,
        TargetIRIs.REACTANT_SPECIES_DICTIONARY.value, TargetIRIs.PRODUCT_SPECIES_DICTIONARY.value,
        TargetIRIs.CATALYST_SPECIES_DICTIONARY.value, TargetIRIs.SOLVENT_SPECIES_DICTIONARY.value,
        TargetIRIs.DOE_TEMPLATE_IRI.value),
    ],
)
def test_get_chemical_reaction_given_iri(initialise_triples, chem_rxn_iri, reactant, product, catalyst, solvent, doe_template_iri):
    sparql_client = initialise_triples
    chem_rxn = sparql_client.get_chemical_reaction_given_iri(chem_rxn_iri)

    assert chem_rxn.instance_iri == chem_rxn_iri
    dict_reactant = {reactant.instance_iri:reactant.hasUniqueSpecies for reactant in chem_rxn.hasReactant}
    assert dict_reactant == reactant
    dict_product = {product.instance_iri:product.hasUniqueSpecies for product in chem_rxn.hasProduct}
    assert dict_product == product
    dict_catalyst = {catalyst.instance_iri:catalyst.hasUniqueSpecies for catalyst in chem_rxn.hasCatalyst}
    assert dict_catalyst == catalyst
    dict_solvent = {solvent.instance_iri:solvent.hasUniqueSpecies for solvent in chem_rxn.hasSolvent}
    assert dict_solvent == solvent
    assert chem_rxn.hasDoETemplate == doe_template_iri

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
    assert hplc_method.retentionTimeMatchThreshold >= 0
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_hplc_method_given_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    hplc_method = sparql_client.get_hplc_method_given_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    internal_standard = sparql_client.get_internal_standard(TargetIRIs.HPLCMETHOD_DUMMY_IRI.value)
    assert hplc_method.instance_iri == TargetIRIs.HPLCMETHOD_DUMMY_IRI.value
    assert hplc_method.usesInternalStandard == internal_standard
    assert hplc_method.retentionTimeMatchThreshold >= 0
    assert all(rf.refersToSpecies is not None for rf in hplc_method.hasResponseFactor)
    assert all(rt.refersToSpecies is not None for rt in hplc_method.hasRetentionTime)

def test_get_chromatogram_point_of_hplc_report(initialise_triples):
    sparql_client = initialise_triples
    list_chrom_pts = sparql_client.get_chromatogram_point_of_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert len(list_chrom_pts) == len(TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value)
    for pt in list_chrom_pts:
        assert pt.instance_iri in TargetIRIs.LIST_CHROMATOGRAMPOINT_IRI.value
        if pt.indicatesComponent is not None:
            assert not pt.unidentified
            assert pt.indicatesComponent.instance_iri is not None
            assert pt.indicatesComponent.representsOccurenceOf is not None
            assert isinstance(pt.indicatesComponent.hasProperty, onto.OntoCAPE_PhaseComponentConcentration)
            assert pt.indicatesComponent.hasProperty.hasValue.numericalValue is not None
        else:
            assert pt.unidentified
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
    assert hplc_report.generatedFor.instance_iri == TargetIRIs.CHEMICAL_AMOUNT_FOR_DUMMY_OUTPUTCHEMICAL_IRI.value
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
    direct_query_hplc_report = sparql_client.get_existing_hplc_report(TargetIRIs.HPLCREPORT_DUMMY_IRI.value)
    assert hplc_job_instance.hasReport.instance_iri == direct_query_hplc_report.instance_iri
    assert hplc_job_instance.hasReport.remoteFilePath == direct_query_hplc_report.remoteFilePath
    assert hplc_job_instance.hasReport.localFilePath == direct_query_hplc_report.localFilePath
    assert hplc_job_instance.hasReport.lastLocalModifiedAt == direct_query_hplc_report.lastLocalModifiedAt
    assert hplc_job_instance.hasReport.lastUploadedAt == direct_query_hplc_report.lastUploadedAt
    assert hplc_job_instance.hasReport.generatedFor.instance_iri == direct_query_hplc_report.generatedFor.instance_iri
    assert hplc_job_instance.hasReport.generatedFor.refersToMaterial == direct_query_hplc_report.generatedFor.refersToMaterial
    assert len(hplc_job_instance.hasReport.records) == len(direct_query_hplc_report.records)
    assert all(pt in direct_query_hplc_report.records for pt in hplc_job_instance.hasReport.records)

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
    "local_file_path,hplc_digital_twin,chemical_amount_iri,internal_standard_species,internal_standard_run_conc,hplc_method_iri",
    [
        (conftest.HPLC_XLS_REPORT_FILE, TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_1_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE, TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_2_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_1_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_2_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_1_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_2_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_1_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_1_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_2_POST_PROC_IRI.value, TargetIRIs.CHEMICAL_AMOUNT_2_POST_PROC_IRI.value, TargetIRIs.ONTOSPECIES_INTERNAL_STANDARD_IRI.value, TargetIRIs.MOLARITY_INTERNAL_STANDARD.value, TargetIRIs.HPLCMETHOD_DUMMY_IRI.value),
    ],
)
def test_upload_download_process_raw_hplc_report(initialise_triples, generate_random_download_path, local_file_path, hplc_digital_twin, chemical_amount_iri, internal_standard_species, internal_standard_run_conc, hplc_method_iri):
    """This is an integration test of five methods that are called by different agents: upload_raw_hplc_report_to_kg, collect_triples_for_hplc_job,
    get_raw_hplc_report_remote_path_and_extension, download_remote_raw_hplc_report, and process_raw_hplc_report."""
    sparql_client = initialise_triples
    timestamp_last_modified = os.path.getmtime(local_file_path)

    # First upload the report and upload relevent triples (as part of HPLC Agent - delegated by VapourtecExecutionAgent)
    hplc_report_iri = sparql_client.upload_raw_hplc_report_to_kg(
        local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified,
        remote_report_subdir=None,
        hplc_digital_twin=hplc_digital_twin
    )

    # Second make the connection between HPLCReport and ChemicalAmount (as part of Execution Agent)
    g = Graph()
    g = sparql_client.collect_triples_for_hplc_job("http://placeholder/rxn_exp_"+str(uuid.uuid4()), chemical_amount_iri, hplc_digital_twin, hplc_report_iri, hplc_method_iri, g)
    sparql_client.uploadGraph(g)

    # Third download uploaded HPLC report file, make sure the content is the same
    remote_file_path, file_extension = sparql_client.get_raw_hplc_report_remote_path_and_extension(hplc_report_iri)
    full_downloaded_path = generate_random_download_path(file_extension)
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Fourth process the raw report (as part of PostProc Agent)
    hplc_report_instance = sparql_client.process_raw_hplc_report(
        hplc_report_iri=hplc_report_iri,
        internal_standard_species=internal_standard_species,
        internal_standard_run_conc_moleperlitre=internal_standard_run_conc,
        temp_local_folder=conftest.DOWNLOADED_DIR,
    )
    assert hplc_report_instance.instance_iri == hplc_report_iri
    assert hplc_report_instance.remoteFilePath == remote_file_path
    assert hplc_report_instance.localFilePath == local_file_path
    assert (hplc_report_instance.lastLocalModifiedAt - timestamp_last_modified) <= 0.00001
    assert hplc_report_instance.lastUploadedAt > hplc_report_instance.lastLocalModifiedAt
    list_chrom_pts = hplc_report_instance.records
    for pt in list_chrom_pts:
        assert pt.instance_iri.startswith(pt.namespace_for_init)
        if pt.indicatesComponent is not None:
            assert not pt.unidentified
            assert pt.indicatesComponent.instance_iri is not None
            assert pt.indicatesComponent.representsOccurenceOf is not None
            assert isinstance(pt.indicatesComponent.hasProperty, onto.OntoCAPE_PhaseComponentConcentration)
            assert pt.indicatesComponent.hasProperty.hasValue.numericalValue is not None
        else:
            assert "unidentified" in pt.rdfs_comment
            assert pt.unidentified
        assert pt.atRetentionTime.hasValue.hasNumericalValue > 0
        assert pt.atRetentionTime.hasValue.hasUnit is not None
        assert pt.hasPeakArea.hasValue.hasNumericalValue > 0
        assert pt.hasPeakArea.hasValue.hasUnit is not None
    dct_phase_comp = {pt.indicatesComponent.instance_iri:pt.indicatesComponent for pt in list_chrom_pts if not pt.unidentified}
    chemical_amount_instance = hplc_report_instance.generatedFor
    assert chemical_amount_instance.instance_iri == chemical_amount_iri
    dct_phase_comp_chemical_amount = {pc.instance_iri:pc for pc in chemical_amount_instance.refersToMaterial.thermodynamicBehaviour.isComposedOfSubsystem}
    assert len(dct_phase_comp) == len(dct_phase_comp_chemical_amount)
    assert all([dct_phase_comp[pc] == dct_phase_comp_chemical_amount[pc] for pc in dct_phase_comp])
    dct_conc_phase_comp = {pc.hasProperty.instance_iri:pc.hasProperty for pc in chemical_amount_instance.refersToMaterial.thermodynamicBehaviour.isComposedOfSubsystem}
    dct_conc_composition = {conc.instance_iri:conc for conc in chemical_amount_instance.refersToMaterial.thermodynamicBehaviour.has_composition.comprisesDirectly}
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

def test_get_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    # this queries all vapourtec rs400s in the triplestore
    vapourtec_rs400_list = sparql_client.get_vapourtec_rs400()
    assert len(vapourtec_rs400_list) >= 1

    # this queries all vapourtec rs400s in the triplestore that are located in a specific lab
    vapourtec_rs400_list = sparql_client.get_vapourtec_rs400(
        list_of_labs_as_constraint=[TargetIRIs.DUMMY_LAB_IRI.value]
    )
    assert len(vapourtec_rs400_list) >= 1
    assert TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value in [vapourtec_rs400.instance_iri for vapourtec_rs400 in vapourtec_rs400_list]

    # this queries a specific vapourtec rs400 in the triplestore
    vapourtec_rs400_list = sparql_client.get_vapourtec_rs400(
        list_vapourtec_rs400_iri=[TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value]
    )
    assert len(vapourtec_rs400_list) == 1
    vapourtec_rs400 = vapourtec_rs400_list[0]
    assert vapourtec_rs400.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value
    assert vapourtec_rs400.manufacturer == TargetIRIs.VAPOURTEC_LTD.value
    assert vapourtec_rs400.isManagedBy is None
    assert vapourtec_rs400.consistsOf is not None
    assert dal.check_if_two_lists_equal(TargetIRIs.VAPOURTECRS400_DUMMY_CONSISTS_OF_LIST.value,
        [r.instance_iri for r in vapourtec_rs400.consistsOf])
    assert vapourtec_rs400.hasState.clz == onto.ONTOVAPOURTEC_IDLE
    assert vapourtec_rs400.hasState.stateLastUpdatedAt == 0
    assert vapourtec_rs400.hasCollectionMethod is not None
    assert vapourtec_rs400.hasCollectionMethod.instance_iri == TargetIRIs.VAPOURTECRS400_DUMMY_COLLECTION_METHOD_IRI.value
    assert vapourtec_rs400.hasCollectionMethod.clz == onto.ONTOVAPOURTEC_SINGLERECEPTACLE
    assert vapourtec_rs400.hasCollectionMethod.toReceptacle == TargetIRIs.VAPOURTECRS400_DUMMY_COLLECTION_METHOD_TO_RECEPTACLE_IRI.value

def test_update_vapourtec_rs400_state(initialise_triples):
    sparql_client = initialise_triples
    # Before update, the state should be Idle
    vapourtec_rs400 = sparql_client.get_vapourtec_rs400(
        list_vapourtec_rs400_iri=[TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value]
    )[0]
    assert vapourtec_rs400.hasState.clz == onto.ONTOVAPOURTEC_IDLE
    assert vapourtec_rs400.hasState.stateLastUpdatedAt == 0

    # After update, the state should be Initialising
    chosen_state = random.choice(onto.LIST_ONTOVAPOURTEC_VALIDSTATE)
    sparql_client.update_vapourtec_rs400_state(
        TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, chosen_state, 1)
    vapourtec_rs400 = sparql_client.get_vapourtec_rs400(
        list_vapourtec_rs400_iri=[TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value]
    )[0]
    assert vapourtec_rs400.hasState.clz == chosen_state
    assert vapourtec_rs400.hasState.stateLastUpdatedAt == 1

    # Nothing should change if the state is not updated
    with pytest.raises(Exception) as e:
        sparql_client.update_vapourtec_rs400_state(
            TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, "http://vapourtec_rs400_state_"+str(uuid.uuid4()), 2)
        assert "is not recognised as a valid state" in str(e.value)
    sparql_client.update_vapourtec_rs400_state(
        TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value, chosen_state, 1)
    vapourtec_rs400 = sparql_client.get_vapourtec_rs400(
        list_vapourtec_rs400_iri=[TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value]
    )[0]
    assert vapourtec_rs400.hasState.clz == chosen_state
    assert vapourtec_rs400.hasState.stateLastUpdatedAt == 1

def test_register_agent_with_hardware(initialise_triples):
    sparql_client = initialise_triples
    agent_iri = "http://"+str(uuid.uuid4())
    hardware_iri = "http://"+str(uuid.uuid4())
    sparql_client.register_agent_with_hardware(
        agent_iri=agent_iri, hardware_digital_twin=hardware_iri)
    assert sparql_client.check_if_triple_exist(
        hardware_iri, onto.ONTOLAB_ISMANAGEDBY, agent_iri)
    # Remove temp agent that manages the digital twin
    sparql_client.performUpdate("""DELETE WHERE {<%s> <%s> <%s>.}""" % (
        hardware_iri, onto.ONTOLAB_ISMANAGEDBY, agent_iri
    ))

def test_connect_hplc_report_with_chemical_amount(initialise_triples):
    sparql_client = initialise_triples
    hplc_report_iri = "http://"+str(uuid.uuid4())
    chemical_amount_iri = "http://"+str(uuid.uuid4())
    sparql_client.connect_hplc_report_with_chemical_amount(
        hplc_report_iri=hplc_report_iri, chemical_amount_iri=chemical_amount_iri)
    assert sparql_client.check_if_triple_exist(
        hplc_report_iri, onto.ONTOHPLC_GENERATEDFOR, chemical_amount_iri)

@pytest.mark.parametrize(
    "local_file_path,hplc_digital_twin",
    [
        (conftest.HPLC_XLS_REPORT_FILE, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_INCOMPLETE, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_UNIDENTIFIED_PEAKS, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
        (conftest.HPLC_XLS_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_1_POST_PROC_IRI.value),
        (conftest.HPLC_TXT_REPORT_FILE_NO_PRODUCT, TargetIRIs.HPLC_2_POST_PROC_IRI.value),
    ],
)
def test_detect_new_hplc_report(initialise_triples, local_file_path, hplc_digital_twin):
    sparql_client = initialise_triples

    # Get the last modified time of the local file and record the start time
    timestamp_last_modified = os.path.getmtime(local_file_path)
    start_timestamp = datetime.now().timestamp()

    # First upload the hplc report to kg
    uploaded_hplc_report = sparql_client.upload_raw_hplc_report_to_kg(
        local_file_path=local_file_path,
        timestamp_last_modified=timestamp_last_modified,
        remote_report_subdir=None,
        hplc_digital_twin=hplc_digital_twin
    )
    time.sleep(2)
    end_timestamp = datetime.now().timestamp()

    # Then check if the hplc report is detected as new
    detected_hplc_report = sparql_client.detect_new_hplc_report(
        hplc_digital_twin=hplc_digital_twin,
        start_timestamp=start_timestamp,
        end_timestamp=end_timestamp
    )

    assert uploaded_hplc_report == detected_hplc_report

def test_get_autosampler(initialise_triples):
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    assert autosampler.instance_iri == TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value

    # Check autosampler sampleLoopVolume is correctly parsed
    assert autosampler.sampleLoopVolume.instance_iri == TargetIRIs.AUTOSAMPLER_SAMPLE_LOOP_VOLUME_IRI.value
    assert autosampler.sampleLoopVolume.hasValue.instance_iri == TargetIRIs.AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_IRI.value
    assert autosampler.sampleLoopVolume.hasValue.hasUnit == TargetIRIs.AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_UNIT.value
    assert autosampler.sampleLoopVolume.hasValue.hasNumericalValue == TargetIRIs.AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_NUM_VAL.value

    # Check all autosampler site is populated correctly
    autosampler_sites = autosampler.hasSite
    assert all([(site.holds.hasFillLevel.hasValue.hasNumericalValue - TargetIRIs.AUTOSAMPLER_LIQUID_LEVEL_DICT.value[site.instance_iri]) < 0.0001 for site in autosampler_sites])
    assert all([site.holds.hasFillLevel.hasValue.hasUnit == TargetIRIs.AUTOSAMPLER_LIQUID_LEVEL_UNIT_DICT.value[site.instance_iri] for site in autosampler_sites])
    assert all([dal.check_if_two_lists_equal([comp.representsOccurenceOf for comp in site.holds.isFilledWith.refersToMaterial.thermodynamicBehaviour.isComposedOfSubsystem],
        TargetIRIs.AUTOSAMPLER_LIQUID_COMPONENT_DICT.value[site.instance_iri]) for site in autosampler_sites if site.holds.isFilledWith is not None])
    assert all([site.locationID == TargetIRIs.AUTOSAMPLER_SITE_LOC_DICT.value[site.instance_iri] for site in autosampler_sites])
    assert all([site.holds.isFilledWith.containsUnidentifiedComponent == TargetIRIs.AUTOSAMPLER_LIQUID_CONTAINS_UNIDENTIFIED_COMPONENT_DICT.value[site.instance_iri] for site in autosampler_sites if site.holds.isFilledWith is not None])

def test_get_autosampler_site_given_input_chemical(initialise_triples):
    """This method tests the get_autosampler_site_given_input_chemical method in ontovapourtec.AutoSampler dataclass."""
    sparql_client = initialise_triples
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    input_chem_lst = sparql_client.get_input_chemical_of_rxn_exp(TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value)[TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value]

    for input_chem in input_chem_lst:
        assert input_chem.instance_iri == TargetIRIs.AUTOSAMPLER_SITE_CHEMICAL_MAPPING_DICT.value[
            autosampler.get_autosampler_site_given_input_chemical(input_chem=input_chem).instance_iri]

def test_update_vapourtec_autosampler_liquid_level_millilitre(initialise_triples):
    sparql_client = initialise_triples

    # First query the vapourtec autosampler liquid level millilitre
    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    # NOTE here we assume the liquid level is in millilitre (as is provided in the test data)
    assert all([site.holds.hasFillLevel.hasValue.hasUnit == onto.OM_MILLILITRE for site in autosampler.hasSite])
    dct_site_loop_volume = {site.instance_iri:site.holds.hasFillLevel.hasValue.hasNumericalValue for site in autosampler.hasSite}

    dct_site_loop_volume_to_change = {site.instance_iri:1 for site in [
        site for site in autosampler.hasSite]}

    # Then update the vapourtec autosampler liquid level millilitre for addition
    sparql_client.update_vapourtec_autosampler_liquid_level_millilitre(
        level_change_of_site=dct_site_loop_volume_to_change,
        for_consumption=False
    )

    # Then check if the vapourtec autosampler liquid level millilitre is updated
    updated_autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    dct_site_loop_volume_updated = {site.instance_iri:site.holds.hasFillLevel.hasValue.hasNumericalValue for site in [
        site for site in updated_autosampler.hasSite]}
    assert all([(dct_site_loop_volume_updated[iri] - (dct_site_loop_volume[iri] + dct_site_loop_volume_to_change[iri])) < 0.0001 for iri in dct_site_loop_volume_updated])

    # Update again but for consumption
    sparql_client.update_vapourtec_autosampler_liquid_level_millilitre(
        level_change_of_site=dct_site_loop_volume_to_change,
        for_consumption=True
    )

    # Check again
    updated_autosampler_again = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    dct_site_loop_volume_updated_again = {site.instance_iri:site.holds.hasFillLevel.hasValue.hasNumericalValue for site in [
        site for site in updated_autosampler_again.hasSite]}

    assert all([(dct_site_loop_volume[iri] - dct_site_loop_volume_updated_again[iri]) < 0.0001 for iri in dct_site_loop_volume_updated_again])

@pytest.mark.parametrize(
    "amount_of_chemical_amount",
    [
        (5),
        (6),
    ],
)
def test_create_chemical_amount_for_reaction_outlet(initialise_triples, amount_of_chemical_amount):
    sparql_client = initialise_triples

    autosampler = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    empty_site = [site.instance_iri for site in autosampler.hasSite if site.holds.isFilledWith is None][0]
    g = sparql_client.create_chemical_amount_for_reaction_outlet(
        autosampler_site_iri=empty_site, amount_of_chemical_amount=amount_of_chemical_amount)

    qres = g.query("""SELECT ?chemical_amount_iri WHERE {?chemical_amount_iri rdf:type <%s>.}""" % onto.ONTOLAB_CHEMICALAMOUNT)
    assert len(qres) == 1
    for row in qres:
        chemical_amount_iri = row.chemical_amount_iri
    autosampler_updated = sparql_client.get_autosampler(TargetIRIs.AUTOSAMPLER_DUMMY_IRI.value)
    dct_site_loop_volume = {site.instance_iri:site.holds.hasFillLevel.hasValue.hasNumericalValue for site in autosampler_updated.hasSite}
    dct_site_chemical_amount = {site.instance_iri:site.holds.isFilledWith.instance_iri for site in autosampler_updated.hasSite if site.holds.isFilledWith is not None}
    assert dct_site_loop_volume[empty_site] == amount_of_chemical_amount
    assert dct_site_chemical_amount[empty_site] == chemical_amount_iri.toPython()

def test_release_vapourtec_rs400_settings(initialise_triples):
    sparql_client = initialise_triples

    vapourtec_rs400_iri = TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value
    assert not sparql_client.performQuery("""ASK {<%s> <%s>* ?hardware. ?settings <%s> ?hardware.}""" % (
        vapourtec_rs400_iri, onto.SAREF_CONSISTSOF, onto.ONTOLAB_SPECIFIES))[0]['ASK']

    setting_iri = "http://"+str(uuid.uuid4())
    sparql_client.performUpdate("""
        INSERT {<%s> <%s> ?hardware.} WHERE {<%s> <%s>* ?hardware.}""" % (
            setting_iri, onto.ONTOLAB_SPECIFIES,
            vapourtec_rs400_iri, onto.SAREF_CONSISTSOF))
    assert sparql_client.performQuery("""ASK {<%s> <%s>* ?hardware. ?settings <%s> ?hardware.}""" % (
        vapourtec_rs400_iri, onto.SAREF_CONSISTSOF, onto.ONTOLAB_SPECIFIES))[0]['ASK']

    sparql_client.release_vapourtec_rs400_settings(vapourtec_rs400_iri=vapourtec_rs400_iri)
    assert not sparql_client.performQuery("""ASK {<%s> <%s>* ?hardware. ?settings <%s> ?hardware.}""" % (
        vapourtec_rs400_iri, onto.SAREF_CONSISTSOF, onto.ONTOLAB_SPECIFIES))[0]['ASK']

def test_upload_and_get_vapourtec_input_file(initialise_triples, generate_random_download_path):
    """Integration test for upload_vapourtec_input_file_to_kg and test_get_vapourtec_input_file."""
    sparql_client = initialise_triples

    local_file_path = conftest.VAPOURTEC_INPUT_FILE
    timestamp_last_modified = os.path.getmtime(local_file_path)
    start_timestamp = datetime.now().timestamp()
    vapourtec_input_file_iri = sparql_client.upload_vapourtec_input_file_to_kg(
        vapourtec_digital_twin=TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value,
        local_file_path=local_file_path,
        remote_file_subdir=None
    )
    end_timestamp = datetime.now().timestamp()

    # Check if the vapourtec input file was uploaded to the kg
    vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri=vapourtec_input_file_iri)
    assert vapourtec_input_file.instance_iri == vapourtec_input_file_iri
    assert (vapourtec_input_file.lastLocalModifiedAt - timestamp_last_modified) <= 0.00001
    assert vapourtec_input_file.lastUploadedAt >= start_timestamp
    assert vapourtec_input_file.lastUploadedAt <= end_timestamp
    assert vapourtec_input_file.localFilePath == local_file_path
    assert sparql_client.fs_url in vapourtec_input_file.remoteFilePath

    # Check that the file uploaded is the same as the test file
    full_downloaded_path = generate_random_download_path('csv')
    sparql_client.downloadFile(remote_file_path=vapourtec_input_file.remoteFilePath, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

def test_get_hplc_given_vapourtec_rs400(initialise_triples):
    sparql_client = initialise_triples
    hplc = sparql_client.get_hplc_given_vapourtec_rs400(vapourtec_rs400_iri=TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert hplc.instance_iri == TargetIRIs.HPLC_DUMMY_IRI.value
    assert hplc.manufacturer == TargetIRIs.HPLC_DUMMY_MANUFACTURER_IRI.value
    assert hplc.isManagedBy is None

    # Register agent with hplc
    agent_iri = "http://"+str(uuid.uuid4())
    sparql_client.register_agent_with_hardware(agent_iri=agent_iri, hardware_digital_twin=TargetIRIs.HPLC_DUMMY_IRI.value)
    hplc = sparql_client.get_hplc_given_vapourtec_rs400(vapourtec_rs400_iri=TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value)
    assert hplc.isManagedBy == agent_iri

    # Remove temp agent that manages the digital twin
    sparql_client.performUpdate("""DELETE WHERE {<%s> <%s> <%s>.}""" % (
        TargetIRIs.HPLC_DUMMY_IRI.value, onto.ONTOLAB_ISMANAGEDBY, agent_iri
    ))

def test_detect_new_hplc_report_from_hplc_derivation(initialise_triples):
    sparql_client = initialise_triples

    hplc_derivation_iri = "http://"+str(uuid.uuid4())
    hplc_job_iri = "http://"+str(uuid.uuid4())
    hplc_report_iri = "http://"+str(uuid.uuid4())
    assert sparql_client.detect_new_hplc_report_from_hplc_derivation(
        hplc_derivation_iri=hplc_derivation_iri) is None

    sparql_client.performUpdate("""INSERT DATA {<%s> <%s> <%s>. <%s> <%s> <%s>.}""" % (
        hplc_job_iri, onto.ONTODERIVATION_BELONGSTO, hplc_derivation_iri,
        hplc_job_iri, onto.ONTOHPLC_HASREPORT, hplc_report_iri
    ))
    assert hplc_report_iri == sparql_client.detect_new_hplc_report_from_hplc_derivation(
        hplc_derivation_iri=hplc_derivation_iri)

@pytest.mark.parametrize(
    "rxnexp_iris,rxn_input_chemical_dict",
    [
        (TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.RXNEXP_OUTPUT_CHEMICAL_DICT.value),
    ],
)
def test_get_output_chemical_of_rxn_exp(initialise_triples, rxnexp_iris, rxn_input_chemical_dict):
    """Also tests get_chemical."""
    sparql_client = initialise_triples
    dict_queried_output_chemical = sparql_client.get_output_chemical_of_rxn_exp(rxnexp_iris)
    for rxnexp_iri in rxnexp_iris:
        list_output_chemical = [out_chem.instance_iri for out_chem in dict_queried_output_chemical[rxnexp_iri]]
        assert len(list_output_chemical) == len(rxn_input_chemical_dict[rxnexp_iri])
        assert len(set(list_output_chemical).difference(set(rxn_input_chemical_dict[rxnexp_iri]))) == 0

@pytest.mark.parametrize(
    "rxn_exp_iri,rxn_con_list,perf_ind_list",
    [
        (TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_2_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_3_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_4_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.EXAMPLE_RXN_EXP_5_IRI.value, TargetIRIs.EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_1_IRI.value, TargetIRIs.NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_2_IRI.value, TargetIRIs.NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST.value),
        (TargetIRIs.NEW_RXN_EXP_3_IRI.value, TargetIRIs.NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST.value, TargetIRIs.NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST.value),
    ],
)
def test_get_rxn_con_or_perf_ind(initialise_triples, rxn_exp_iri, rxn_con_list, perf_ind_list):
    """This test tests get_reaction_condition and get_performance_indicator of ontoreaction.ReactionExperiment dataclass."""
    sparql_client = initialise_triples

    rxn_exp = sparql_client.getReactionExperiment(rxn_exp_iri)[0]

    for clz in [onto.ONTOREACTION_RESIDENCETIME, onto.ONTOREACTION_REACTIONPRESSURE,
        onto.ONTOREACTION_REACTIONSCALE, onto.ONTOREACTION_REACTIONTEMPERATURE, onto.ONTOREACTION_STOICHIOMETRYRATIO]:
        positional_id_lst = TargetIRIs.RXN_EXP_REACTION_CONDITION_CLZ_POSITIONAL_ID_DICT.value[rxn_exp_iri][clz]
        for positional_id in positional_id_lst:
            identified_cond = rxn_exp.get_reaction_condition(clz=clz, positional_id=positional_id)
            assert identified_cond.instance_iri in rxn_con_list
            assert identified_cond.positionalID == positional_id
            assert identified_cond.clz == clz
            if identified_cond.instance_iri in TargetIRIs.RXN_EXP_REACTION_CONDITION_POSITIONAL_ID_DICT.value[rxn_exp_iri]:
                assert identified_cond.positionalID == TargetIRIs.RXN_EXP_REACTION_CONDITION_POSITIONAL_ID_DICT.value[rxn_exp_iri][identified_cond.instance_iri]

    if perf_ind_list is not None:
        for clz in [onto.ONTOREACTION_YIELD, onto.ONTOREACTION_RUNMATERIALCOST]:
            identified_ind = rxn_exp.get_performance_indicator(clz=clz, positional_id=None)
            assert identified_ind.instance_iri in perf_ind_list
            assert identified_ind.positionalID == None
            assert identified_ind.clz == clz

def test_get_reagent_bottle(initialise_triples):
    sparql_client = initialise_triples

    reagent_bottle = sparql_client.get_reagent_bottle(TargetIRIs.REAGENTBOTTLE_1_DUMMY_IRI.value)
    assert reagent_bottle.instance_iri == TargetIRIs.REAGENTBOTTLE_1_DUMMY_IRI.value
    assert reagent_bottle.isFilledWith.instance_iri is not None
    assert reagent_bottle.hasFillLevel.hasValue.hasNumericalValue > 0
    assert reagent_bottle.hasFillLevel.hasValue.hasUnit == onto.OM_MILLILITRE
    assert reagent_bottle.hasWarningLevel.hasValue.hasNumericalValue > 0
    assert reagent_bottle.hasWarningLevel.hasValue.hasUnit == onto.OM_MILLILITRE
    assert reagent_bottle.hasMaxLevel.hasValue.hasNumericalValue > 0
    assert reagent_bottle.hasMaxLevel.hasValue.hasUnit == onto.OM_MILLILITRE
    assert reagent_bottle.hasFillLevel.hasValue.hasNumericalValue <= reagent_bottle.hasMaxLevel.hasValue.hasNumericalValue

def test_get_all_rxn_exp_given_chem_rxn(initialise_triples):
    sparql_client = initialise_triples

    lst_rxn_exp = sparql_client.get_all_rxn_exp_given_chem_rxn(TargetIRIs.CHEMICAL_REACTION_IRI.value)
    assert dal.check_if_two_lists_equal(
        lst_rxn_exp,
        TargetIRIs.LIST_EXAMPLE_RXN_EXP.value + TargetIRIs.LIST_NEW_RXN_EXP.value + TargetIRIs.LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP.value
    )

def test_get_r4_reactor_rxn_exp_assigned_to(initialise_triples):
    sparql_client = initialise_triples

    rxn_exp_iri = "http://"+str(uuid.uuid4())
    reactor_iri = "http://"+str(uuid.uuid4())
    dict_reactor_before = sparql_client.get_r4_reactor_rxn_exp_assigned_to(rxn_exp_iri)
    assert len(dict_reactor_before) == 1
    assert dict_reactor_before[rxn_exp_iri] is None

    sparql_client.performUpdate("""INSERT DATA {<%s> <%s> <%s>.}""" % (
        rxn_exp_iri, onto.ONTOREACTION_ISASSIGNEDTO, reactor_iri
    ))
    dict_reactor_after = sparql_client.get_r4_reactor_rxn_exp_assigned_to(rxn_exp_iri)
    assert len(dict_reactor_after) == 1
    assert reactor_iri == dict_reactor_after[rxn_exp_iri]

def test_create_equip_settings_for_rs400_from_rxn_exp(initialise_triples):
    """This method tests create_equip_settings_for_rs400_from_rxn_exp method of ontovapoutrec.VapourtecRS400 dataclass."""
    sparql_client = initialise_triples

    rxn_exp = sparql_client.getReactionExperiment(TargetIRIs.EXAMPLE_RXN_EXP_1_IRI.value)[0]
    rs400 = sparql_client.get_vapourtec_rs400(
        list_vapourtec_rs400_iri=[TargetIRIs.VAPOURTECRS400_DUMMY_IRI.value]
    )[0]

    lst_equip_settings = rs400.create_equip_settings_for_rs400_from_rxn_exp(rxn_exp=rxn_exp)

    # Generated equip settings should be generated for the reaction experiment
    assert all([stg.wasGeneratedFor == rxn_exp.instance_iri for stg in lst_equip_settings])
    # Generated equip settings should contain both ReactorSettings and PumpSettings
    assert all([isinstance(stg, onto.ReactorSettings) or isinstance(stg, onto.PumpSettings) for stg in lst_equip_settings])
    # Generated equip settings' parameter settings should all point to the reaction condition in the reaction experiment
    all([param.hasQuantity.instance_iri in [rxn_con.instance_iri for rxn_con in rxn_exp.hasReactionCondition] for setting in lst_equip_settings for param in setting.get_parameter_settings()])

def test_get_species_molar_mass_kilogrampermole(initialise_triples):
    sparql_client = initialise_triples

    for species_iri in TargetIRIs.SPECIES_MOLAR_MASS_DICT.value:
        molar_mass = sparql_client.get_species_molar_mass_kilogrampermole(species_iri)
        assert molar_mass == TargetIRIs.SPECIES_MOLAR_MASS_DICT.value[species_iri]

@pytest.mark.parametrize(
    "hplc_report_iri,local_file_path,hplc_report_extension",
    [
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_TXT_REPORT_FILE, onto.TXTFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_XLS_REPORT_FILE, onto.XLSFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_TXT_REPORT_FILE_INCOMPLETE, onto.TXTFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_XLS_REPORT_FILE_INCOMPLETE, onto.XLSFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_TXT_REPORT_FILE_UNIDENTIFIED_PEAKS, onto.TXTFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_XLS_REPORT_FILE_UNIDENTIFIED_PEAKS, onto.XLSFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_TXT_REPORT_FILE_NO_PRODUCT, onto.TXTFILE_EXTENSION),
        (TargetIRIs.HPLCREPORT_DUMMY_IRI.value, conftest.HPLC_XLS_REPORT_FILE_NO_PRODUCT, onto.XLSFILE_EXTENSION),
    ],
)
def test_get_matching_species_from_hplc_results(initialise_triples, hplc_report_iri, local_file_path, hplc_report_extension):
    sparql_client = initialise_triples

    list_points = hplc.read_raw_hplc_report_file(
        hplc_report_iri=hplc_report_iri,
        file_path=local_file_path,
        filename_extension=hplc_report_extension
    )

    # get the instance of HPLCMethod
    hplc_method = sparql_client.get_hplc_method_given_hplc_report(hplc_report_iri)

    # map them to chromatogram point (qury phase component based on hplc method, and hplc)
    dct_points = {sparql_client.get_matching_species_from_hplc_results(
        pt.get(onto.ONTOHPLC_RETENTIONTIME), hplc_method
    ):pt for pt in list_points if sparql_client.get_matching_species_from_hplc_results(
        pt.get(onto.ONTOHPLC_RETENTIONTIME), hplc_method
    ) is not None}

    assert all([dct_points[key][onto.ONTOHPLC_RETENTIONTIME].hasValue.hasNumericalValue == TargetIRIs.HPLC_DUMMAY_REPORT_FILE_SPECIES_RETENTION_TIME_IDENTIFY.value[key] for key in dct_points])

def test_collect_triples_for_performance_indicators(initialise_triples):
    sparql_client = initialise_triples

    lst_performance_indicator = []
    for i in range(random.randint(1, 100)):
        lst_performance_indicator.append(
            onto.PerformanceIndicator(
                instance_iri="http://"+str(uuid.uuid4()),
                clz="http://"+str(uuid.uuid4()),
                rxn_exp_iri="http://"+str(uuid.uuid4()),
                objPropWithExp=["http://"+str(uuid.uuid4()), "http://"+str(uuid.uuid4())],
                hasValue=onto.OM_Measure(
                    instance_iri="http://"+str(uuid.uuid4()),
                    hasUnit="http://"+str(uuid.uuid4()),
                    hasNumericalValue=random.uniform(0, 100)
                ),
                positionalID=random.randint(0, 100),
                yieldLimitingSpecies="http://"+str(uuid.uuid4())
            ))

    g = Graph()
    g = sparql_client.collect_triples_for_performance_indicators(
        lst_performance_indicator=lst_performance_indicator, g=g)
    for performance_indicator in lst_performance_indicator:
        assert (URIRef(performance_indicator.instance_iri), None, None) in g
        assert (None, None, URIRef(performance_indicator.clz)) in g
        assert (URIRef(performance_indicator.rxn_exp_iri), None, None) in g
        assert all([(None, URIRef(o), None) in g for o in performance_indicator.objPropWithExp])
        assert (URIRef(performance_indicator.hasValue.instance_iri), None, None) in g
        assert (None, None, URIRef(performance_indicator.hasValue.hasUnit)) in g
        assert (None, None, Literal(performance_indicator.hasValue.hasNumericalValue)) in g
        assert (None, None, Literal(performance_indicator.positionalID)) in g
        assert (None, None, URIRef(performance_indicator.yieldLimitingSpecies)) in g

def test_collect_triples_for_output_chemical_of_chem_amount(initialise_triples):
    sparql_client = initialise_triples

    lst_phase_component = []
    lst_phase_component_conc = []
    for i in range(random.randint(1, 100)):
        phase_component_conc = onto.OntoCAPE_PhaseComponentConcentration(
            instance_iri="http://"+str(uuid.uuid4()),
            hasValue=onto.OntoCAPE_ScalarValue(
                instance_iri="http://"+str(uuid.uuid4()),
                hasUnitOfMeasure="http://"+str(uuid.uuid4()),
                numericalValue=random.uniform(0, 100)
            )
        )
        lst_phase_component_conc.append(phase_component_conc)
        lst_phase_component.append(onto.OntoCAPE_PhaseComponent(
            instance_iri="http://"+str(uuid.uuid4()),
            hasProperty=phase_component_conc,
            representsOccurenceOf="http://"+str(uuid.uuid4())
        ))
    chemical_iri = "http://"+str(uuid.uuid4())
    chemical_amount = onto.ChemicalAmount(
        instance_iri="http://"+str(uuid.uuid4()),
        refersToMaterial=onto.Chemical(
            instance_iri=chemical_iri,
            thermodynamicBehaviour=onto.OntoCAPE_SinglePhase(
                instance_iri="http://"+str(uuid.uuid4()),
                hasStateOfAggregation=onto.OntoCAPE_StateOfAggregation(
                    instance_iri="http://"+str(uuid.uuid4())),
                isComposedOfSubsystem=lst_phase_component,
                has_composition=onto.OntoCAPE_Composition(
                    instance_iri="http://"+str(uuid.uuid4()),
                    comprisesDirectly=lst_phase_component_conc,
                ),
                representsThermodynamicBehaviorOf=chemical_iri
            )
        ),
        fills="http://"+str(uuid.uuid4()),
    )
    rxn_exp_iri = "http://"+str(uuid.uuid4())

    g = Graph()
    g = sparql_client.collect_triples_for_output_chemical_of_chem_amount(
        chemical_amount=chemical_amount,
        rxn_exp_iri=rxn_exp_iri,
        g=g
    )

    assert (URIRef(chemical_amount.instance_iri), URIRef(onto.ONTOCAPE_REFERSTOMATERIAL), URIRef(chemical_amount.refersToMaterial.instance_iri)) in g
    assert (URIRef(rxn_exp_iri), URIRef(onto.ONTOREACTION_HASOUTPUTCHEMICAL), URIRef(chemical_amount.refersToMaterial.instance_iri)) in g
    assert if_object_collected_in_graph(g, chemical_amount.refersToMaterial)

def test_collect_triples_for_hplc_job(initialise_triples):
    sparql_client = initialise_triples

    g = Graph()
    rxn_exp_iri = "http://"+str(uuid.uuid4())
    chemical_amount_iri = "http://"+str(uuid.uuid4())
    hplc_digital_twin = "http://"+str(uuid.uuid4())
    hplc_report_iri = "http://"+str(uuid.uuid4())
    hplc_method_iri = "http://"+str(uuid.uuid4())

    g = sparql_client.collect_triples_for_hplc_job(
        rxn_exp_iri=rxn_exp_iri, chemical_amount_iri=chemical_amount_iri,
        hplc_digital_twin=hplc_digital_twin, hplc_report_iri=hplc_report_iri,
        hplc_method_iri=hplc_method_iri, g=g
    )

    qres = g.query("""SELECT ?hplc_job_iri WHERE {?hplc_job_iri rdf:type <%s>.}""" % onto.ONTOHPLC_HPLCJOB)
    assert len(qres) == 1
    for row in qres:
        hplc_job_iri = row.hplc_job_iri
    assert (URIRef(hplc_digital_twin), URIRef(onto.ONTOHPLC_HASJOB), URIRef(hplc_job_iri)) in g
    assert (URIRef(hplc_job_iri), URIRef(onto.ONTOHPLC_CHARACTERISES), URIRef(rxn_exp_iri)) in g
    assert (URIRef(hplc_job_iri), URIRef(onto.ONTOHPLC_USESMETHOD), URIRef(hplc_method_iri)) in g
    assert (URIRef(hplc_job_iri), URIRef(onto.ONTOHPLC_HASREPORT), URIRef(hplc_report_iri)) in g
    assert (URIRef(hplc_report_iri), URIRef(onto.ONTOHPLC_GENERATEDFOR), URIRef(chemical_amount_iri)) in g

def test_collect_triples_for_chromatogram_point(initialise_triples):
    sparql_client = initialise_triples

    lst_chromatogram_point = []
    for i in range(random.randint(1, 100)):
        lst_chromatogram_point.append(
            onto.ChromatogramPoint(
                instance_iri="http://"+str(uuid.uuid4()),
                indicatesComponent=onto.OntoCAPE_PhaseComponent(
                    instance_iri="http://"+str(uuid.uuid4()),
                    hasProperty=onto.OntoCAPE_PhaseComponentConcentration(
                        instance_iri="http://"+str(uuid.uuid4()),
                        hasValue=onto.OntoCAPE_ScalarValue(
                            instance_iri="http://"+str(uuid.uuid4()),
                            hasUnitOfMeasure="http://"+str(uuid.uuid4()),
                            numericalValue=random.uniform(0, 1)
                        ),
                    ),
                    representsOccurenceOf="http://"+str(uuid.uuid4())
                ),
                hasPeakArea=onto.PeakArea(
                    instance_iri="http://"+str(uuid.uuid4()),
                    hasValue=onto.OM_Measure(
                        instance_iri="http://"+str(uuid.uuid4()),
                        hasUnit="http://"+str(uuid.uuid4()),
                        hasNumericalValue=random.uniform(1, 100)
                    )
                ),
                atRetentionTime=onto.RetentionTime(
                    instance_iri="http://"+str(uuid.uuid4()),
                    hasValue=onto.OM_Measure(
                        instance_iri="http://"+str(uuid.uuid4()),
                        hasUnit="http://"+str(uuid.uuid4()),
                        hasNumericalValue=random.uniform(1, 100)
                    ),
                )
            )
        )

    hplc_report_iri="http://"+str(uuid.uuid4())
    g = Graph()
    g = sparql_client.collect_triples_for_chromatogram_point(
        chrom_pts=lst_chromatogram_point, hplc_report_iri=hplc_report_iri, g=g
    )

    for pt in lst_chromatogram_point:
        assert (URIRef(hplc_report_iri), URIRef(onto.ONTOHPLC_RECORDS), URIRef(pt.instance_iri)) in g
        assert if_object_collected_in_graph(g, pt)

def test_get_all_laboratories(initialise_triples):
    sparql_client = initialise_triples
    labs = sparql_client.get_all_laboratories()
    assert len(labs) == 1
    assert labs[0] == TargetIRIs.DUMMY_LAB_IRI.value

@pytest.mark.parametrize(
    "rxn_con_list,expected_rxn",
    [
        (onto.ONTOREACTION_YIELD, TargetIRIs.LIST_EXAMPLE_RXN_EXP.value + TargetIRIs.LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP.value),
        ([onto.ONTOREACTION_YIELD], TargetIRIs.LIST_EXAMPLE_RXN_EXP.value + TargetIRIs.LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP.value),
        ([onto.ONTOREACTION_YIELD, onto.ONTOREACTION_RUNMATERIALCOST], TargetIRIs.LIST_EXAMPLE_RXN_EXP.value),
        ([onto.ONTOREACTION_YIELD, onto.ONTOREACTION_SPACETIMEYIELD], []),
        ([onto.ONTOREACTION_YIELD, onto.ONTOREACTION_SPACETIMEYIELD, onto.ONTOREACTION_RUNMATERIALCOST], []),
        ([onto.ONTOREACTION_SPACETIMEYIELD], []),
    ],
)
def test_get_all_rxn_exp_with_target_perfind_given_chem_rxn(initialise_triples, rxn_con_list, expected_rxn):
    sparql_client = initialise_triples
    res = sparql_client.get_all_rxn_exp_with_target_perfind_given_chem_rxn(
        TargetIRIs.CHEMICAL_REACTION_IRI.value,
        rxn_con_list
    )
    assert dal.check_if_two_lists_equal(res, expected_rxn)

@pytest.mark.skip(reason="TODO")
def test_locate_possible_input_chemical(initialise_triples):
    pass

@pytest.mark.skip(reason="TODO after proper representation of species density")
def test_get_species_density(initialise_triples):
    sparql_client = initialise_triples
    sparql_client = ChemistryAndRobotsSparqlClient()
    pass

@pytest.mark.skip(reason="TODO after proper representation of species material cost")
def test_get_species_material_cost(initialise_triples):
    sparql_client = initialise_triples
    sparql_client = ChemistryAndRobotsSparqlClient()
    pass

@pytest.mark.skip(reason="TODO after proper representation of species eco score")
def test_get_species_eco_score(initialise_triples):
    sparql_client = initialise_triples
    sparql_client = ChemistryAndRobotsSparqlClient()
    pass

def if_object_collected_in_graph(g: Graph, ontology_object: onto.BaseOntology):
    if (URIRef(ontology_object.instance_iri), None, None) not in g:
        print("{} not collected in graph".format(ontology_object.instance_iri))
        return False
    for key in ontology_object.__fields__:
        if getattr(ontology_object, key) is not None:
            if isinstance(getattr(ontology_object, key), onto.BaseOntology):
                return if_object_collected_in_graph(g, getattr(ontology_object, key))
            elif isinstance(getattr(ontology_object, key), list):
                for item in getattr(ontology_object, key):
                    if item is not None:
                        if isinstance(item, onto.BaseOntology):
                            return if_object_collected_in_graph(g, item)
                        elif isinstance(item, str):
                            if (None, None, URIRef(item)) not in g and (None, None, Literal(item)) not in g:
                                print("key {} {} not collected in graph".format(key, item))
                                return False
                        elif isinstance(item, int) or isinstance(item, float):
                            if (None, None, Literal(item)) not in g:
                                print("key {} {} not collected in graph".format(key, item))
                                return False
            elif isinstance(getattr(ontology_object, key), str):
                if (None, None, URIRef(getattr(ontology_object, key))) not in g and (None, None, Literal(getattr(ontology_object, key))) not in g:
                    print("key {} {} not collected in graph".format(key, getattr(ontology_object, key)))
                    return False
            elif isinstance(getattr(ontology_object, key), int) or isinstance(getattr(ontology_object, key), float):
                if (None, None, Literal(getattr(ontology_object, key))) not in g:
                    print("key {} {} not collected in graph".format(key, getattr(ontology_object, key)))
                    return False
    return True
