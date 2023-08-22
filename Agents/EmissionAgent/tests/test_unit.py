################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 22 Aug 2023                            #
################################################

# This module contains a few unit tests for the emission agent

import pytest

from py4jps import agentlogging

from . import conftest as cf


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('dev')


@pytest.mark.skip(reason="Test not implemented yet")
def test_extraction_of_emission_input_value():
    """
    This function tests whether the correct time series values are extracted
    from ConsumedGas and ProvidedHeat time series for given SimulationTime
    """
    #TODO: implement
    pass


@pytest.mark.parametrize(
    "derivation_input_set, expected_error_msg",
    [
        (cf.ERRONEOUS_INPUTS_1, cf.ERRONEOUS_MSG_1),
        (cf.ERRONEOUS_INPUTS_2, cf.ERRONEOUS_MSG_2),
        (cf.ERRONEOUS_INPUTS_3, cf.ERRONEOUS_MSG_3),
        (cf.ERRONEOUS_INPUTS_4, cf.ERRONEOUS_MSG_4),
        (cf.ERRONEOUS_INPUTS_5, cf.ERRONEOUS_MSG_5),
        (cf.ERRONEOUS_INPUTS_6, cf.ERRONEOUS_MSG_6)
    ],
)
def test_validate_input_values(
    create_example_agent, derivation_input_set, expected_error_msg
):
    """
    Test whether emission agent detects invalid input markups as expected
    """
    
    # Create agent instance without registration in KG
    agent = create_example_agent(register_agent=False)

    with pytest.raises(TypeError) as exc_info:
        # Directly call input validation function and assert exception type
        agent.validate_input_values(inputs=derivation_input_set, derivationIRI='TestDerivation')

    # Check if expected error message is raised
    assert expected_error_msg in str(exc_info.value)
    