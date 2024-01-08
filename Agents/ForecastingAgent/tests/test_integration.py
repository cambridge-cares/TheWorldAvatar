#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 25 Jul 2023                             #
#################################################

# The purpose of this module is to test the Agent Flask App. HTTP requests are sent 
# to the Flask App and the response is checked.

import copy
import pytest
import requests
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
from rdflib import Graph
from rdflib import RDF
from operator import eq, gt

from darts.models import TFTModel

from py4jps import agentlogging

import forecastingagent.datamodel as dm
from forecastingagent.agent.forcasting_config import DOUBLE, BOOLEAN, TIME_FORMAT, \
                                                     create_model_dict
from forecastingagent.fcmodels import load_pretrained_model

from . import conftest as cf


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


#@pytest.mark.skip(reason="")
def test_example_triples():
    """
    This test checks that the example triples are correct in syntax.

    Raises:
        e: If the example triples are not valid RDF.
    """
    g = Graph()
    pathlist = Path(cf.TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        try:
            g.parse(str(path))
        except Exception as e:
            raise e


#@pytest.mark.skip(reason="")
def test_example_data_instantiation(initialise_clients):
    """
    This test checks that all example data gets correctly instantiated,
    including associated time series data in PostgreSQL.
    """
    # Get required clients from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_url) == 0

    # Initialise and upload time series
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_1,
                              times=cf.TIMES, values=cf.VALUES_1,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_url) == 2
    assert sparql_client.getAmountOfTriples() == (triples + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    times, values = ts_client.retrieve_timeseries(cf.IRI_TO_FORECAST_1)
    assert times == cf.TIMES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES_1

    # Verify that dropping all tables works as expected
    cf.clear_database(rdb_url)
    assert cf.get_number_of_rdb_tables(rdb_url) == 0


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "derivation_input_set, dataIRI, input_chunk_length, with_unit, overwrite_forecast, ts_times, ts_values, case",
    [
        (cf.DERIVATION_INPUTS_1, cf.ASSOCIATED_DATAIRI_1, cf.DURATION_1, True, True, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_1),
        (cf.DERIVATION_INPUTS_1, cf.ASSOCIATED_DATAIRI_1, cf.DURATION_1, True, False, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_2),
        (cf.DERIVATION_INPUTS_2, cf.IRI_TO_FORECAST_2, cf.DURATION_1, True, True, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_3),
        (cf.DERIVATION_INPUTS_2, cf.IRI_TO_FORECAST_2, cf.DURATION_1, True, False, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_4),
        (cf.DERIVATION_INPUTS_3, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, True, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_5),
        (cf.DERIVATION_INPUTS_3, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, False, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_6),
        (cf.DERIVATION_INPUTS_4, cf.IRI_TO_FORECAST_3, cf.DURATION_2, False, True, cf.TIMES, cf.VALUES_3, cf.TEST_CASE_7),
        (cf.DERIVATION_INPUTS_4, cf.IRI_TO_FORECAST_3, cf.DURATION_2, False, False, cf.TIMES, cf.VALUES_3, cf.TEST_CASE_8),
    ],
)
def test_create_forecast(
    initialise_clients, create_example_agent, derivation_input_set, dataIRI, input_chunk_length,
    with_unit, overwrite_forecast, ts_times, ts_values, case
):
    """
    Test if Forecasting Agent performs derivation update as expected (using 
    default Prophet model without covariates)
        - forecasts are created using Prophet
        - historical data length (same as input_chunk_length for non-neural method):
            336/8760h (HIST_DURATION_1, DURATION_1 or HIST_DURATION_2, DURATION_2)
        - initial interval: OptimisationInterval_1
                            Jan 01 2020 00:00:00 UTC - Jan 02 2020 00:00:00 UTC
        - updated interval: OptimisationInterval_2
                            Jan 02 2020 00:00:00 UTC - Jan 03 2020 00:00:00 UTC
    """

    # Get forecast agent IRI for current test case
    if overwrite_forecast:
        agent_iri = cf.AGENT_w_OVERWRITING_IRI
        agent_url = cf.AGENT_w_OVERWRITING_URL
    else:
        agent_iri = cf.AGENT_wo_OVERWRITING_IRI
        agent_url = cf.AGENT_wo_OVERWRITING_URL

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=dataIRI,
                              times=ts_times, values=ts_values,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Register derivation agent in KG
    # - Successful agent registration within the KG is required to create/pick up derivations
    # - Hence, the dockerised agents are started without initial registration and registration
    #   is done within the test to guarantee that test Blazegraph will be ready
    # - The "belated" registration of the dockerised agents can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    create_example_agent(ontoagent_service_iri=agent_iri,
                         ontoagent_http_url=agent_url) 

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.TS_FORECAST)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.FORECAST_TRIPLES                                        # triples for new forecast
    if with_unit:
        triples += cf.UNIT_TRIPLES
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 3    # number of inputs + derivation type + associated agent + belongsTo
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there is 1 derivation output (i.e. Forecast IRI)
    assert len(derivation_outputs) == 1
    assert dm.TS_FORECAST in derivation_outputs
    assert len(derivation_outputs[dm.TS_FORECAST]) == 1

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_1 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_1 - 3600
    assert outp_interval['start_unix'] == cf.T_1
    assert outp_interval['end_unix'] == cf.T_2

    # Assess initial forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                      name=case)
    print(f'Forecast errors for case: {case}')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    # Update derivation interval and add latest timestamp to trigger update
    cf.update_derivation_interval(derivation_iri, cf.FC_INTERVAL_2, sparql_client)
    assert sparql_client.getAmountOfTriples() == triples
    derivation_client.addTimeInstanceCurrentTimestamp(cf.FC_INTERVAL_2)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT
    assert sparql_client.getAmountOfTriples() == triples

    # Request for derivation update and verify that no new triples have been added,
    # only time series and interval values have been amended
    derivation_client.unifiedUpdateDerivation(derivation_iri)
    if not overwrite_forecast:
        triples += cf.FORECAST_TRIPLES          # triples for new forecast
        if with_unit:
            triples += cf.UNIT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Retrieve updated forecast details
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_2 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_2 - 3600
    assert outp_interval['start_unix'] == cf.T_2
    assert outp_interval['end_unix'] == cf.T_3
    
    # Assess updated forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                      name=case+'_updated')
    print(f'Forecast errors for case: {case}_updated')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    print("All check passed.")


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "http_request, fail, equal, expected_result",
    [
        (cf.ERROR_REQUEST, False, True, eq),
        (cf.ERROR_REQUEST, False, False, gt),
        (cf.ERRONEOUS_ERROR_REQUEST_1, True, None, cf.ERRONEOUS_ERROR_MSG_1),
        (cf.ERRONEOUS_ERROR_REQUEST_2, True, None, cf.ERRONEOUS_ERROR_MSG_2),
        (cf.ERRONEOUS_ERROR_REQUEST_3, True, None, cf.ERRONEOUS_ERROR_MSG_3),
    ],
)
def test_evaluate_forecast(
    initialise_clients, http_request, fail, equal, expected_result
):
    """
    Test if forecast errors are evaluated as expected

    Boolean flags:
        - fail: True if the test is expected to fail
        - equal: True if time series is compared with itself
    """

    # Get required clients from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_1,
                              times=cf.TIMES, values=cf.VALUES_1,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)
    ts_client.init_timeseries(dataIRI=cf.IRI_TO_FORECAST_2,
                              times=cf.TIMES, values=cf.VALUES_2,
                              ts_type=DOUBLE, time_format=TIME_FORMAT)

    if not fail:
        # Retrieve list of instantiated time series IRIs
        tsIRIs = sparql_client.get_all_tsIRIs()
        if equal:
            http_request['query']['dataIRI_target'] = sparql_client.get_dataIRI(tsIRIs[0])
            http_request['query']['dataIRI_fc'] = sparql_client.get_dataIRI(tsIRIs[0])
        else:
            http_request['query']['dataIRI_target'] = sparql_client.get_dataIRI(tsIRIs[0])
            http_request['query']['dataIRI_fc'] = sparql_client.get_dataIRI(tsIRIs[1])

    # Create HTTP request to evaluate forecast errors
    headers = {'Content-Type': 'application/json'}
    agent_base_url = cf.AGENT_w_OVERWRITING_URL[:cf.AGENT_w_OVERWRITING_URL.rfind('/')+1]
    url = agent_base_url + '/evaluate_errors'
    response = requests.post(url, json=http_request, headers=headers)

    if fail:
        # Verify that correct error message is returned for erroneous requests
        assert response.status_code == 500
        assert expected_result in response.text

    else:
        # Check successful execution/response
        assert response.status_code == 200
        response = response.json()
        assert expected_result(response['mape'], 0)
        assert expected_result(response['smape'], 0)
        assert expected_result(response['mse'], 0)
        assert expected_result(response['rmse'], 0)
        assert expected_result(response['max_error'], 0)


@pytest.mark.skip(reason="Test will fail if the model is not available at the given link")
def test_load_pretrained_model(initialise_clients):
    """
    Test the function `load_pretrained_model` to load a pretrained model from 
    a PyTorch checkpoint and model file
    """

    # Get required clients from fixture
    sparql_client, _, _, _ = initialise_clients
    # Initialise all triples in test_triples
    cf.initialise_triples(sparql_client)

    # Retrieve instantiated model details for pretrained model
    model = sparql_client.get_fcmodel_details(cf.FORECASTING_MODEL_2)
    # Create relevant entries of overarching forecasting configuration
    cfg = {
        'fc_model': create_model_dict(model)
    }

    # Test if pretrained model is loaded correctly
    model = load_pretrained_model(cfg, TFTModel, force_download=True)    
    assert model.__class__.__name__ == 'TFTModel'
    assert model.model.input_chunk_length == 168
    assert model.model.output_chunk_length == 24
    
    # Use previously downloaded model
    model = load_pretrained_model(cfg, TFTModel, force_download=False)    
    assert model.__class__.__name__ == 'TFTModel'
    assert model.model.input_chunk_length == 168
    assert model.model.output_chunk_length == 24


@pytest.mark.skip(reason="Test will fail if the model is not available at the given link")
@pytest.mark.parametrize(
    "derivation_input_set, heatDemand, input_chunk_length, with_unit, overwrite_forecast, ts_times, covariates, case",
    [
        (cf.DERIVATION_INPUTS_5, cf.ASSOCIATED_DATAIRI_1, cf.DURATION_3, True, True, cf.TIMES, cf.COVARIATES_1, cf.TEST_CASE_9),
        (cf.DERIVATION_INPUTS_5, cf.ASSOCIATED_DATAIRI_1, cf.DURATION_3, True, False, cf.TIMES, cf.COVARIATES_1, cf.TEST_CASE_10),
        (cf.DERIVATION_INPUTS_6, cf.IRI_TO_FORECAST_2, cf.DURATION_3, True, True, cf.TIMES, cf.COVARIATES_1, cf.TEST_CASE_11),
        (cf.DERIVATION_INPUTS_6, cf.IRI_TO_FORECAST_2, cf.DURATION_3, True, False, cf.TIMES, cf.COVARIATES_1, cf.TEST_CASE_12)
    ],
)
def test_create_tft_forecast(
    initialise_clients, create_example_agent, derivation_input_set, heatDemand, input_chunk_length,
    with_unit, overwrite_forecast, ts_times, covariates, case
):
    """
    Test if forecasting agent performs derivation update as expected for 
    pre-trained temporal fusion transformer model 
    (as required for district heating optimisation use case)

    NOTE: historical_data_length (marked up Duration instance) and input_chunk_length
          are not equivalent for pre-trained neural method:
          - historical_data_length: used to scale input data (and covariates)
          - input_chunk_length: length of historical data used to generate (next) forecast
    """

    # Get forecast agent IRI for current test case
    if overwrite_forecast:
        agent_iri = cf.AGENT_w_OVERWRITING_IRI
        agent_url = cf.AGENT_w_OVERWRITING_URL
    else:
        agent_iri = cf.AGENT_wo_OVERWRITING_IRI
        agent_url = cf.AGENT_wo_OVERWRITING_URL

    # Generate test time series data (incl. required covariates)
    try:
        # Prio1: Try extracting meaningful test data from actual historical data
        df = pd.read_csv(cf.DH_DATA)
        df.set_index(pd.to_datetime(df['Date']).dt.strftime(TIME_FORMAT), inplace=True)
        df2 = df.loc[ts_times]
        test_data = {
            heatDemand: (df2['Waermeeinspeisung (MW)'].values.astype(float), DOUBLE),
            # air temprature (float)
            covariates[0]: (df2['Aussentemperatur (degC)'].values.astype(float), DOUBLE),
            # public holiday (boolean)
            # NOTE: Creation of py4jps/TimeSeriesClient compliant boolean type 
            #       not straight forward;hence, this complicated expression
            covariates[1]: (list(map(bool,(df2['Holiday'] | df2['Vacation']).values.astype(int))), BOOLEAN)
        }
    except:
        # Prio2: Create random test data in case download of actual data fails
        test_data = {
            heatDemand: (cf.generate_bounded_random_walk(5, len(ts_times), 0, 0.5, 0, 20), DOUBLE),
            # air temprature (float)
            covariates[0]: (cf.generate_bounded_random_walk(10, len(ts_times), 0, 1, -10, 30), DOUBLE),
            # public holiday (boolean)
            covariates[1]: ([bool(x) for x in np.random.randint(0, 2, len(ts_times))], BOOLEAN)
        }

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples repository
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Initialise time series in KG and RDB
    for k, v in test_data.items():
        ts_client.init_timeseries(dataIRI=k, times=ts_times, values=v[0],
                                  ts_type=v[1], time_format=TIME_FORMAT)
    triples += len(test_data) * cf.TS_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Register derivation agent in KG
    create_example_agent(ontoagent_service_iri=agent_iri, ontoagent_http_url=agent_url) 

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.TS_FORECAST)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.FORECAST_TRIPLES                                        # triples for new forecast
    if with_unit:
        triples += cf.UNIT_TRIPLES
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 3    # number of inputs + derivation type + associated agent + belongsTo
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there is 1 derivation output (i.e. Forecast IRI)
    assert len(derivation_outputs) == 1
    assert dm.TS_FORECAST in derivation_outputs
    assert len(derivation_outputs[dm.TS_FORECAST]) == 1

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_1 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_1 - 3600
    assert outp_interval['start_unix'] == cf.T_1
    assert outp_interval['end_unix'] == cf.T_2

    # Assess initial forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(heatDemand, fcIRI, ts_client, agent_url=agent_url,
                                      name=case)
    print(f'Forecast errors for case: {case}')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    # Update derivation interval and add latest timestamp to trigger update
    cf.update_derivation_interval(derivation_iri, cf.FC_INTERVAL_2, sparql_client)
    assert sparql_client.getAmountOfTriples() == triples
    derivation_client.addTimeInstanceCurrentTimestamp(cf.FC_INTERVAL_2)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT
    assert sparql_client.getAmountOfTriples() == triples

    # Request for derivation update and verify that no new triples have been added,
    # only time series and interval values have been amended
    derivation_client.unifiedUpdateDerivation(derivation_iri)
    if not overwrite_forecast:
        triples += cf.FORECAST_TRIPLES          # triples for new forecast
        if with_unit:
            triples += cf.UNIT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Retrieve updated forecast details
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_2 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_2 - 3600
    assert outp_interval['start_unix'] == cf.T_2
    assert outp_interval['end_unix'] == cf.T_3
    
    # Assess updated forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(heatDemand, fcIRI, ts_client, agent_url=agent_url,
                                      name=case+'_updated')
    print(f'Forecast errors for case: {case}_updated')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    print("All check passed.")


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "derivation_input_set, dataIRI, input_chunk_length, with_unit, overwrite_forecast, ts_times, covariates, case",
    [
        (cf.DERIVATION_INPUTS_7, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, True, cf.TIMES, cf.COVARIATES_2, cf.TEST_CASE_13),
        (cf.DERIVATION_INPUTS_8, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, True, cf.TIMES, cf.COVARIATES_2, cf.TEST_CASE_14),
        (cf.DERIVATION_INPUTS_9, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, True, cf.TIMES, cf.COVARIATES_3, cf.TEST_CASE_15),
        (cf.DERIVATION_INPUTS_10, cf.IRI_TO_FORECAST_1, cf.DURATION_2, False, True, cf.TIMES, cf.COVARIATES_3, cf.TEST_CASE_16),
    ],
)
def test_create_prophet_covariates_forecast(
    initialise_clients, create_example_agent, derivation_input_set, dataIRI, input_chunk_length,
    with_unit, overwrite_forecast, ts_times, covariates, case
):
    """
    Test if Forecasting Agent performs derivation update as expected (using 
    default Prophet model with covariates)
        - forecasts are created using Prophet
        - historical data length (same as input_chunk_length for non-neural method):
            336/8760h (HIST_DURATION_1, DURATION_1 or HIST_DURATION_2, DURATION_2)
        - initial interval: OptimisationInterval_1
                            Jan 01 2020 00:00:00 UTC - Jan 02 2020 00:00:00 UTC
        - updated interval: OptimisationInterval_2
                            Jan 02 2020 00:00:00 UTC - Jan 03 2020 00:00:00 UTC
    """

    # Get forecast agent IRI for current test case
    if overwrite_forecast:
        agent_iri = cf.AGENT_w_OVERWRITING_IRI
        agent_url = cf.AGENT_w_OVERWRITING_URL
    else:
        agent_iri = cf.AGENT_wo_OVERWRITING_IRI
        agent_url = cf.AGENT_wo_OVERWRITING_URL

    # Generate synthetic test time series data (incl. required covariates)

    # Promotion on every 6th and 7th time unit in a cycle of 7
    covariate1 = [float(1) if i % 7 == 6 or i % 7 == 5 else float(0) for i in range(len(ts_times))]
    # Special Event on every 9th and 24th time unit in a cycle of 25
    covariate2 = [float(1) if i % 25 == 9 or i % 25 == 24 else float(0) for i in range(len(ts_times))]
    # Baseline sales with a linear trend
    sales = [50 + i for i in range(len(ts_times))]
    # Increase sales for promotion and special event
    sales = [sale + (20 * promo) for sale, promo in zip(sales, covariate1)]
    sales = [float(sale + (50 * event)) for sale, event in zip(sales, covariate2)]

    # Create test data dictionary depending on number of covariates
    test_data = {dataIRI: sales, covariates[0]: covariate1} if len(covariates) == 1 else {dataIRI: sales, covariates[0]: covariate1, covariates[1]: covariate2}

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples repository
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Initialise time series in KG and RDB
    for k, v in test_data.items():
        ts_client.init_timeseries(dataIRI=k, times=ts_times, values=v,
                                  ts_type=DOUBLE, time_format=TIME_FORMAT)
    triples += len(test_data) * cf.TS_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Register derivation agent in KG
    create_example_agent(ontoagent_service_iri=agent_iri, ontoagent_http_url=agent_url) 

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.TS_FORECAST)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.FORECAST_TRIPLES                                        # triples for new forecast
    if with_unit:
        triples += cf.UNIT_TRIPLES
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 3    # number of inputs + derivation type + associated agent + belongsTo
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there is 1 derivation output (i.e. Forecast IRI)
    assert len(derivation_outputs) == 1
    assert dm.TS_FORECAST in derivation_outputs
    assert len(derivation_outputs[dm.TS_FORECAST]) == 1

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_1 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_1 - 3600
    assert outp_interval['start_unix'] == cf.T_1
    assert outp_interval['end_unix'] == cf.T_2

    # Assess initial forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                      name=case)
    print(f'Forecast errors for case: {case}')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    # Update derivation interval and add latest timestamp to trigger update
    cf.update_derivation_interval(derivation_iri, cf.FC_INTERVAL_2, sparql_client)
    assert sparql_client.getAmountOfTriples() == triples
    derivation_client.addTimeInstanceCurrentTimestamp(cf.FC_INTERVAL_2)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT
    assert sparql_client.getAmountOfTriples() == triples

    # Request for derivation update and verify that no new triples have been added,
    # only time series and interval values have been amended
    derivation_client.unifiedUpdateDerivation(derivation_iri)
    if not overwrite_forecast:
        triples += cf.FORECAST_TRIPLES          # triples for new forecast
        if with_unit:
            triples += cf.UNIT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Retrieve updated forecast details
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_2 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_2 - 3600
    assert outp_interval['start_unix'] == cf.T_2
    assert outp_interval['end_unix'] == cf.T_3
    
    # Assess updated forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                      name=case+'_updated')
    print(f'Forecast errors for case: {case}_updated')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    print("All check passed.")


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "derivation_input_set1, derivation_input_set2, fcmodelIRI1, fcmodelIRI2, dataIRI, ts_times, covariates, case1, case2",
    [
        (cf.DERIVATION_INPUTS_3, cf.DERIVATION_INPUTS_7, cf.FORECASTING_MODEL_1, cf.FORECASTING_MODEL_3, cf.IRI_TO_FORECAST_1,  cf.TIMES,
         cf.COVARIATES_2, cf.TEST_CASE_5+'_covariates_comparison', cf.TEST_CASE_13+'_covariates_comparison'),
    ],
)
def test_significance_covariates_forecast(
    initialise_clients, create_example_agent, derivation_input_set1, derivation_input_set2, fcmodelIRI1, fcmodelIRI2, dataIRI, 
    ts_times, covariates, case1, case2
):
    """
    Test if Forecasting Agent performs better with covariates (using 
    default Prophet model with covariates)
    Forecasting with covariates is case-specific, so this test is for
    an example constructed to be forecasted more accurately with covariates
        - forecasts are created using Prophet
        - historical data length (same as input_chunk_length for non-neural method):
            336/8760h (HIST_DURATION_1, DURATION_1 or HIST_DURATION_2, DURATION_2)
        - initial interval: OptimisationInterval_1
                            Jan 01 2020 00:00:00 UTC - Jan 02 2020 00:00:00 UTC
        - updated interval: OptimisationInterval_2
                            Jan 02 2020 00:00:00 UTC - Jan 03 2020 00:00:00 UTC
    derivation_input_set1, fcmodelIRI1 and case1 are for the model without covariates
    derivation_input_set2, fcmodelIRI2 and case2 are for the model with covariates
    """

    # Get forecast agent IRI for current test case
    agent_iri = cf.AGENT_w_OVERWRITING_IRI
    agent_url = cf.AGENT_w_OVERWRITING_URL

    # Generate synthetic test time series data (incl. required covariates)
    # Promotion on every 6th and 7th time unit in a cycle of 7
    covariate1 = [float(1) if i % 7 == 6 or i % 7 == 5 else float(0) for i in range(len(ts_times))]
    # Special Event on every 9th and 24th time unit in a cycle of 25
    covariate2 = [float(1) if i % 25 == 9 or i % 25 == 24 else float(0) for i in range(len(ts_times))]
    # Baseline sales with a linear trend
    sales = [50 + i for i in range(len(ts_times))]
    # Increase sales for promotion and special event
    sales = [sale + (20 * promo) for sale, promo in zip(sales, covariate1)]
    sales = [float(sale + (50 * event)) for sale, event in zip(sales, covariate2)]

    # Create test data dictionary
    test_data = {dataIRI: sales, covariates[0]: covariate1, covariates[1]: covariate2}

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples repository
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)

    # Verify that model1 and model2 are witout and with covariates
    model1 = sparql_client.get_fcmodel_details(fcmodelIRI1)
    model2 = sparql_client.get_fcmodel_details(fcmodelIRI2)
    assert not "covariates" in model1.keys()
    assert len(model2['covariates']) > 0
    
    # Initialise time series in KG and RDB
    for k, v in test_data.items():
        ts_client.init_timeseries(dataIRI=k, times=ts_times, values=v,
                                  ts_type=DOUBLE, time_format=TIME_FORMAT)

    # 1) Get forecast of agent without covariates

    # Register derivation agents in KG
    create_example_agent(ontoagent_service_iri=agent_iri, ontoagent_http_url=agent_url) 

    # Create derivation instances for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set1,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")

    # Query input & output of the derivation instance
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]

    # Assess initial forecast error and create plot for visual inspection
    errors1 = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                       name=case1)    
    ts1 = ts_client.retrieve_timeseries(fcIRI)
    
    print(f'Forecast errors for case: {case1}')
    for k,v in errors1.items():
        print(f'{k}: {round(v,5)}')

    # 2) Get forecast of agent with covariates

    # Initialise all triples in test_triples repository
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)

    # Initialise time series in KG and RDB
    for k, v in test_data.items():
        ts_client.init_timeseries(dataIRI=k, times=ts_times, values=v,
                                  ts_type=DOUBLE, time_format=TIME_FORMAT)

    # Register derivation agents in KG
    create_example_agent(ontoagent_service_iri=agent_iri, ontoagent_http_url=agent_url) 

    # Create derivation instances for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set2,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")

    # Query input & output of the derivation instance
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]

    # Assess initial forecast error and create plot for visual inspection
    errors2 = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                       name=case2)    
    ts2 = ts_client.retrieve_timeseries(fcIRI)

    print(f'Forecast errors for case: {case2}')
    for k,v in errors2.items():
        print(f'{k}: {round(v,5)}')    
        # Check if forecast with covariates is better than without
        assert round(errors2[k], 5) <= round(errors1[k], 5)

    # Plot the two forecasts for visual comparison
    target = ts_client.retrieve_timeseries(dataIRI)
    df_target = pd.DataFrame({'timestamp': target[0], 'actual data': target[1]})
    df1 = pd.DataFrame({'timestamp': ts1[0], 'without covariates': ts1[1]})
    df2 = pd.DataFrame({'timestamp': ts2[0], 'with covariates': ts2[1]})
    # Convert 'timestamp' column to a datetime data type
    df_target['timestamp'] = pd.to_datetime(df_target['timestamp'])
    df_target.set_index('timestamp', inplace=True)
    df1['timestamp'] = pd.to_datetime(df1['timestamp'])
    df1.set_index('timestamp', inplace=True)
    df2['timestamp'] = pd.to_datetime(df2['timestamp'])
    df2.set_index('timestamp', inplace=True)
    # Merge DataFrames (while keeping all historical data) and slice to relevant period
    df = pd.merge(df_target, pd.merge(df1, df2, on='timestamp', how='outer'), on='timestamp', how='outer')
    offset1 = pd.DateOffset(days=3)
    offset2 = pd.DateOffset(days=1)
    valid_indices = (df.index - offset2 <= df2.index.max()) & (df.index + offset1 >= df2.index.min())
    valid_entries = df[valid_indices]

    # Create new figure, plot and save to volume
    ax = valid_entries.plot()
    ax.set_xlabel('Timestamp')
    ax.set_ylabel('Values')
    ax.set_title("Prophet Forecasting with and without Covariates")
    ax.grid(which='minor', axis='both')
    fp = '/app/tests/test_plots/' + 'Prophet_comparison_with_and_without_covariates' + '.png'
    plt.savefig(fp)

    print("All check passed.")


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "derivation_input_set, dataIRI, dataIRIs, input_chunk_length, with_unit, ts_times, ts_values, case",
    [
        (cf.DERIVATION_INPUTS_1, cf.ASSOCIATED_DATAIRI_1, [cf.ASSOCIATED_DATAIRI_1, cf.NOT_ASSOCIATED_DATAIRI_2], cf.DURATION_1, True, cf.TIMES, cf.VALUES_1, cf.TEST_CASE_18),
        (cf.DERIVATION_INPUTS_4, cf.IRI_TO_FORECAST_3, [cf.IRI_TO_FORECAST_3, cf.IRI_TO_FORECAST_4], cf.DURATION_2, False, cf.TIMES, cf.VALUES_3, cf.TEST_CASE_19),        
    ],
)
def test_multiple_datairis_to_same_tsiri(
    initialise_clients, create_example_agent, derivation_input_set, dataIRI, dataIRIs, input_chunk_length,
    with_unit, ts_times, ts_values, case
):
    """
    Test if Forecasting Agent works when multiple dataIRIs are linked to the 
    same time series IRI (e.g. they refer to different columns of the same table)
        - forecasts are created using Prophet
        - historical data length (same as input_chunk_length for non-neural method):
            336/8760h (HIST_DURATION_1, DURATION_1 or HIST_DURATION_2, DURATION_2)
        - initial interval: OptimisationInterval_1
                            Jan 01 2020 00:00:00 UTC - Jan 02 2020 00:00:00 UTC
        - updated interval: OptimisationInterval_2
                            Jan 02 2020 00:00:00 UTC - Jan 03 2020 00:00:00 UTC
    """

    # Get forecasting agent instance (i.e., use default with forecast overwriting!)
    agent_iri = cf.AGENT_w_OVERWRITING_IRI
    agent_url = cf.AGENT_w_OVERWRITING_URL

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)

    # Create ts with multiple columns, i.e., multiple dataIRIs associated with same tsIRI
    with ts_client.connect() as conn:
        ts_client.tsclient.initTimeSeries(dataIRIs, [DOUBLE]*len(dataIRIs), TIME_FORMAT, conn)
        ts = ts_client.create_timeseries(ts_times, dataIRIs, [ts_values]*2)
        ts_client.tsclient.addTimeSeriesData(ts, conn)                          

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES
    # Account for "additional" hasTimeSeries relationships for all associated dataIRIs
    triples += len(dataIRIs) - 1
    assert sparql_client.getAmountOfTriples() == triples

    # Register derivation agent in KG
    create_example_agent(ontoagent_service_iri=agent_iri,
                         ontoagent_http_url=agent_url) 

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.TS_FORECAST)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, derivation_input_set,
                                                                  dm.ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.FORECAST_TRIPLES                                        # triples for new forecast
    if with_unit:
        triples += cf.UNIT_TRIPLES
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 3    # number of inputs + derivation type + associated agent + belongsTo
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there is 1 derivation output (i.e. Forecast IRI)
    assert len(derivation_outputs) == 1
    assert dm.TS_FORECAST in derivation_outputs
    assert len(derivation_outputs[dm.TS_FORECAST]) == 1

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Retrieve instantiated forecast and verify its details
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_1 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_1 - 3600
    assert outp_interval['start_unix'] == cf.T_1
    assert outp_interval['end_unix'] == cf.T_2

    # Assess initial forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url, 
                                      name=case)
    print(f'Forecast errors for case: {case}')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    # Update derivation interval and add latest timestamp to trigger update
    cf.update_derivation_interval(derivation_iri, cf.FC_INTERVAL_2, sparql_client)
    assert sparql_client.getAmountOfTriples() == triples
    derivation_client.addTimeInstanceCurrentTimestamp(cf.FC_INTERVAL_2)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT
    assert sparql_client.getAmountOfTriples() == triples

    # Request for derivation update and verify that no new triples have been added,
    # only time series and interval values have been amended
    derivation_client.unifiedUpdateDerivation(derivation_iri)
    assert sparql_client.getAmountOfTriples() == triples

    # Retrieve updated forecast details
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
    fc_intervals = sparql_client.get_forecast_details(fcIRI)
    inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
    outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
    assert inp_interval['start_unix'] == cf.T_2 - input_chunk_length*3600
    assert inp_interval['end_unix'] == cf.T_2 - 3600
    assert outp_interval['start_unix'] == cf.T_2
    assert outp_interval['end_unix'] == cf.T_3
    
    # Assess updated forecast error and create plot for visual inspection
    errors = cf.assess_forecast_error(dataIRI, fcIRI, ts_client, agent_url=agent_url,
                                      name=case+'_updated')
    print(f'Forecast errors for case: {case}_updated')
    for k,v in errors.items():
        print(f'{k}: {round(v,5)}')

    print("All check passed.")