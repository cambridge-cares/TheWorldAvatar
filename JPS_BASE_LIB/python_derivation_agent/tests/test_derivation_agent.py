import time

import tests.utils as utils

def test_integration_test(initialise_agent):
    sparql_client, derivation_client, rng_agent, min_agent, max_agent, diff_agent = initialise_agent

    all_instances = utils.initialise_triples_assert_pure_inputs(
        sparql_client=sparql_client,
        derivation_client=derivation_client,
        # Do NOT delete all triples, as we have just registered agent instances
        # Instead, the triples are deleted as part of initialise_agent fixture
        delete_all_triples=False
    )

    # Start the scheduler to monitor derivations
    rng_agent.add_job_monitoring_derivations(start=True)
    min_agent.add_job_monitoring_derivations(start=True)
    max_agent.add_job_monitoring_derivations(start=True)
    diff_agent.add_job_monitoring_derivations(start=True)

    # Create derivation instance given above information, the timestamp of this derivation is 0
    rng_derivation_iri = rng_agent.derivationClient.createAsyncDerivationForNewInfo(
        rng_agent.agentIRI,
        [all_instances.IRI_UPPER_LIMIT, all_instances.IRI_LOWER_LIMIT, all_instances.IRI_NUM_OF_PTS]
    )
    max_derivation_iri = max_agent.derivationClient.createAsyncDerivationForNewInfo(
        max_agent.agentIRI, [rng_derivation_iri]
    )
    min_derivation_iri = min_agent.derivationClient.createAsyncDerivationForNewInfo(
        min_agent.agentIRI, [rng_derivation_iri]
    )
    diff_derivation_iri = diff_agent.derivationClient.createAsyncDerivationForNewInfo(
        diff_agent.agentIRI, [max_derivation_iri, min_derivation_iri]
    )

    # Check if the derivation instance is created correctly
    assert sparql_client.checkInstanceClass(
        rng_derivation_iri,
        sparql_client.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN
    )
    assert sparql_client.checkInstanceClass(
        max_derivation_iri,
        sparql_client.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN
    )
    assert sparql_client.checkInstanceClass(
        min_derivation_iri,
        sparql_client.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN
    )
    assert sparql_client.checkInstanceClass(
        diff_derivation_iri,
        sparql_client.jpsBaseLib_view.DerivationSparql.ONTODERIVATION_DERIVATIONASYN
    )

    ################################################################################
    ########## Stage 1: check if new information generation is successful ##########
    ################################################################################

    # Query timestamp of the each derivation every 8 seconds until it's updated
    #######################
    ## I. RNG derivation ##
    #######################
    rng_derivation_current_timestamp = 0
    while rng_derivation_current_timestamp == 0:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        rng_derivation_current_timestamp = utils.get_timestamp(
            rng_derivation_iri, sparql_client)

    # Test that if the knowledge graph contains the correct number of points
    # Get the number of points
    numofpoints = sparql_client.getValue(sparql_client.getNumOfPoints())
    # Get the list of actual points generated
    pt_dict = sparql_client.getPointsInList(sparql_client.getListOfPoints())
    # Check if the number of generated points is the same as the input
    assert len(pt_dict) == numofpoints
    # Also check if the number of generated points matches the total number of points in the knowledge graph
    pt_in_kg = sparql_client.getPointsInKG()
    assert len(pt_in_kg) == numofpoints

    # Get the max and min vlaue of the generated list of points
    all_pt_values = pt_dict.values()
    max_value = max(all_pt_values)
    min_value = min(all_pt_values)

    # Get the upper limit
    upperlimit_iri = sparql_client.getUpperLimit()
    upperlimit = sparql_client.getValue(upperlimit_iri)
    # Get the lower limit
    lowerlimit_iri = sparql_client.getLowerLimit()
    lowerlimit = sparql_client.getValue(lowerlimit_iri)

    # Check if the max and min generated values are within the upper and lower limits defined in inputs
    assert max_value <= upperlimit
    assert lowerlimit <= min_value

    ########################
    ## II. Max derivation ##
    ########################
    max_derivation_current_timestamp = 0
    while max_derivation_current_timestamp == 0:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        max_derivation_current_timestamp = utils.get_timestamp(
            max_derivation_iri, sparql_client)

    # Check if the max value is calculated correctly
    queried_max = sparql_client.getValue(sparql_client.getMaxValueIRI())
    assert queried_max == max_value

    #########################
    ## III. Min derivation ##
    #########################
    min_derivation_current_timestamp = 0
    while min_derivation_current_timestamp == 0:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        min_derivation_current_timestamp = utils.get_timestamp(
            min_derivation_iri, sparql_client)

    # Check if the max value is calculated correctly
    queried_min = sparql_client.getValue(sparql_client.getMinValueIRI())
    assert queried_min == min_value

    #########################
    ## IV. Diff derivation ##
    #########################
    diff_derivation_current_timestamp = 0
    while diff_derivation_current_timestamp == 0:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        diff_derivation_current_timestamp = utils.get_timestamp(
            diff_derivation_iri, sparql_client)

    # Check if the difference is calculated correctly
    queried_diff = sparql_client.getValue(sparql_client.getDifferenceIRI())
    assert queried_diff == max_value - min_value

    ####################################################################################
    ########## Stage 2: modify the value of pure input and request for update ##########
    ####################################################################################

    #############################
    ## I. Modify num of points ##
    #############################
    sparql_client.increaseNumOfPointsByOne()
    rng_agent.derivationClient.updateTimestamp(sparql_client.getNumOfPoints())
    assert sparql_client.getValue(
        sparql_client.getNumOfPoints()) == numofpoints + 1

    ###############################
    ## II. Request for an update ##
    ###############################
    diff_agent.derivationClient.unifiedUpdateDerivation(diff_derivation_iri)

    # Wait until the difference derivation is updated
    ts = utils.get_timestamp(diff_derivation_iri, sparql_client)
    diff_derivation_current_timestamp = ts
    while diff_derivation_current_timestamp == ts:
        time.sleep(8)
        # the queried results must be converted to int, otherwise it will never equal to 0
        diff_derivation_current_timestamp = utils.get_timestamp(
            diff_derivation_iri, sparql_client)

    # Check if all the information in the knowledge graph is complete and correct
    numofpoints = sparql_client.getValue(sparql_client.getNumOfPoints())
    # Get the list of actual points generated
    pt_dict = sparql_client.getPointsInList(sparql_client.getListOfPoints())
    assert len(pt_dict) == numofpoints
    # Also check if the number of generated points matches the total number of points in the knowledge graph
    pt_in_kg = sparql_client.getPointsInKG()
    assert len(pt_in_kg) == numofpoints

    # Get the max and min vlaue of the generated list of points
    all_pt_values = pt_dict.values()
    max_value = max(all_pt_values)
    min_value = min(all_pt_values)

    # Get the upper limit
    upperlimit_iri = sparql_client.getUpperLimit()
    upperlimit = sparql_client.getValue(upperlimit_iri)
    # Get the lower limit
    lowerlimit_iri = sparql_client.getLowerLimit()
    lowerlimit = sparql_client.getValue(lowerlimit_iri)

    # Check if the max and min generated values are within the upper and lower limits defined in inputs
    assert max_value <= upperlimit
    assert lowerlimit <= min_value

    # Check if the max value is calculated correctly
    queried_max = sparql_client.getValue(sparql_client.getMaxValueIRI())
    assert queried_max == max_value

    # Check if the max value is calculated correctly
    queried_min = sparql_client.getValue(sparql_client.getMinValueIRI())
    assert queried_min == min_value

    # Check if the difference is calculated correctly
    queried_diff = sparql_client.getValue(sparql_client.getDifferenceIRI())
    assert queried_diff == max_value - min_value
