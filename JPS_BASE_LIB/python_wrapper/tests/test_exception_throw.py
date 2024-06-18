import pytest
import time

from . import utils as utils
from . import conftest as cft
from .agents.sparql_client_for_test import RANDOM_EXAMPLE_EXCEPTION_THROW_MSG


@pytest.mark.parametrize(
    "local_test",
    [ # True - derivation created for local agent instance; False - derivation created for agent in docker
        (True),
        (False),
    ],
)
def test_exception_throw(initialise_clients_and_agents_for_exception_throw, local_test):
    sparql_client, derivation_client = initialise_clients_and_agents_for_exception_throw

    input_placeholder_exception_throw_iri = utils.initialise_triples_for_exception_throw_test(sparql_client)

    # Create exception_throw_agent
    exception_throw_agent = cft.create_exception_throw_agent(
        cft.EXCEPTIONTHROW_ENV,
        sparql_client.query_endpoint,
        True,
        in_docker=False,
        random_agent_iri=local_test,
    )

    # Create derivation instance given above information, the timestamp of this derivation is 0
    exception_throw_derivation_iri_list = []
    for i in range(3):
        iri = derivation_client.createAsyncDerivationForNewInfo(
            exception_throw_agent.agentIRI,
            [input_placeholder_exception_throw_iri]
        )

        # Check if the derivation instance is created correctly
        assert sparql_client.check_instance_class(
            iri,
            sparql_client.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN
        )

        exception_throw_derivation_iri_list.append(iri)

    # Start the scheduler to monitor derivations
    if local_test:
        exception_throw_agent.start_all_periodical_job()

    # Wait for four period
    time.sleep(4 * exception_throw_agent.time_interval)

    # Check that the status of derivations should be flipped to Error:
    # if the amount of derivations in Error status matches the amount of total derivations got marked up
    # then it implies the agent was able to catch the exception and proceed to next derivation without getting stuck
    exc_throw_derivations = derivation_client.derivation_client.getDerivationsAndStatusType(exception_throw_agent.agentIRI)
    assert len(exc_throw_derivations) == 3
    assert all([str(exc_throw_derivations[d]) == 'ERROR' for d in exc_throw_derivations])
    # also all of the error message recorded in rdfs:comment should have the error message defined in the ExceptionThrowAgent
    err_derivations = derivation_client.derivation_client.getDerivationsInErrorStatus(exception_throw_agent.agentIRI)
    assert len(err_derivations) == 3
    assert all([RANDOM_EXAMPLE_EXCEPTION_THROW_MSG in d.getErrMsg() for d in err_derivations])

    # Stop teh scheduler
    if local_test:
        exception_throw_agent.scheduler.shutdown()
