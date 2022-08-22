import pytest
import time

import rxnoptgoaliteragent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Test cases for sparql_client
# ----------------------------------------------------------------------------------

@pytest.mark.parametrize(
    "goal_set_iri",
    [
        (cf.IRIs.GOALSET_1.value),
    ],
)
def test_(initialise_test_triples, goal_set_iri):
    sparql_client = initialise_test_triples
    sparql_client.get_goal_set_instance(goal_set_iri)
    time.sleep(200)
