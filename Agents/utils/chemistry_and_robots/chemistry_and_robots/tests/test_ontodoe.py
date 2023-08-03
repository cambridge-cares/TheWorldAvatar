import pytest

from chemistry_and_robots.tests.conftest import TargetIRIs
import chemistry_and_robots.kg_operations.dict_and_list as dal

@pytest.mark.parametrize(
    "doe_domain_iri, rxn_exp_list, expected_rxn_after_filter",
    [
        (TargetIRIs.DOE_DOMAIN_IRI.value, TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.LIST_EXAMPLE_RXN_EXP.value),
        (TargetIRIs.DOE_DOMAIN_IRI.value, TargetIRIs.LIST_NEW_RXN_EXP.value, []), # No performance indicator so all will be filtered out
        (TargetIRIs.DOE_DOMAIN_IRI.value, TargetIRIs.LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP.value, []),
        (TargetIRIs.DOE_TEMPLATE_DOMAIN_IRI.value, TargetIRIs.LIST_EXAMPLE_RXN_EXP.value, TargetIRIs.LIST_EXAMPLE_RXN_EXP.value),
        (TargetIRIs.DOE_TEMPLATE_DOMAIN_IRI.value, TargetIRIs.LIST_NEW_RXN_EXP.value, []), # No performance indicator so all will be filtered out
        (TargetIRIs.DOE_TEMPLATE_DOMAIN_IRI.value, TargetIRIs.LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP.value, []),
        (TargetIRIs.DOE_TEMPLATE_DOMAIN_IRI.value, None, []),
        (TargetIRIs.DOE_TEMPLATE_DOMAIN_IRI.value, [], []),
    ],
)
def test_filter_reaction_experiment_as_beliefs(initialise_triples, doe_domain_iri, rxn_exp_list, expected_rxn_after_filter):
    sparql_client = initialise_triples
    doe = sparql_client.getDoEDomain(doe_domain_iri)
    rxn_exp = doe.filter_reaction_experiment_as_beliefs(sparql_client.getReactionExperiment(rxn_exp_list))
    assert dal.check_if_two_lists_equal(
        [exp.instance_iri for exp in rxn_exp],
        expected_rxn_after_filter
    )
