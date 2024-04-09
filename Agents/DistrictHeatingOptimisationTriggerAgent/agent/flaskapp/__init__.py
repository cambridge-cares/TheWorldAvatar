################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 09 Oct 2022                            #
################################################

# Create Flask application and define HTTP route to initiate district heating
# optimisation by derivation agents and trigger subsequent runs by updating inputs

import json
import requests
from pathlib import Path
from celery import Celery
from flask import Flask, request, jsonify

from py4jps import agentlogging
from pyderivationagent import PyDerivationClient

import agent.flaskapp.tasks as tasks
from agent.datamodel import *
from agent.kgutils.kgclient import KGClient
from agent.utils.agent_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                      FORECASTING_AGENT_IRI, DH_OPTIMISATION_AGENT_IRI, \
                                      EMISSION_ESTIMATION_AGENT_IRI, \
                                      DISPERSION_INTERACTOR_URL

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


# Create and start Flask app
app = Flask(__name__)

# Upload triples from .ttl files and add covariates to forecasting models
kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
tasks.upload_triples(kg_client)

# Launch celery task queue
celery = Celery(app.name, broker='redis://localhost:6379/0')


@app.route('/triggerOptimisation', methods=['POST'])
def trigger_optimisation():
    # Check if previous optimisation task is still running
    if is_processing_task_running():
            return jsonify(message='Previous request is not finished yet. Please try again later.'), 423
    
    # Otherwise, initiate optimisation task
    try:
        # Verify received HTTP request parameters
        params = request.get_json()
        params = tasks.validate_input_params(params)

        # Queue the optimisation task
        #NOTE: For debugging please switch (un-)commented in next two lines
        #trigger_optimisation_task(params)
        task_id = trigger_optimisation_task.apply_async(args=[params])

        return jsonify(message=f'District heating optimisation task started with ID: {task_id}'), 200
    
    except Exception:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)

        # Return an error response
        return jsonify(message='An error occurred during optimisation. See agent log for details.'), 500


#NOTE: For debugging please comment next line
@celery.task
def trigger_optimisation_task(params):
    try:
        # Initialise sparql and derivation clients
        kg_client = KGClient(query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        derivation_client = PyDerivationClient(
            derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
            query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT)
        
        # Check for already instantiated chain of derivations (to be reused)
        logger.info('Checking for already instantiated derivations ...')
        # 1) Heat demand and grid temp forecast derivation IRIs; returns [] if not exist
        fc_deriv_iris = kg_client.get_forecast_derivations()
        # 2) Downstream derivation IRIs; returns [] if not exist
        # Generation optimisation derivation
        opti_deriv_iri = [] if not fc_deriv_iris else \
                         kg_client.get_downstream_derivation(fc_deriv_iris[0])
        if len(opti_deriv_iri) > 1:
            tasks.raise_value_error(f'More than 1 Generation optimisation derivation retrieved: {", ".join(opti_deriv_iri)}')
        # Emission estimation derivations
        em_deriv_iri = [] if not opti_deriv_iri else \
                       kg_client.get_downstream_derivation(opti_deriv_iri[0])
        if len(em_deriv_iri) > 2:
            tasks.raise_value_error(f'More than 2 Emission estimation derivation retrieved: {", ".join(em_deriv_iri)}')
        derivs = opti_deriv_iri + em_deriv_iri
        
        try:
            # Retrieve unique time inputs (i.e., optimisation interval, simulation time)
            # attached to retrieved derivations; throws Exception if any does not exists
            sim_t, opti_int, opti_t1, opti_t2 = kg_client.get_pure_trigger_inputs(derivs)
            print('Derivation chain already instantiated.')
            new_derivation = False
        except:
            new_derivation = True
            print('Instantiating new chain of derivations ...')
            # Create IRIs for time inputs to instantiate
            sim_t, opti_int, opti_t1, opti_t2, heat_length, tmp_length, freq = \
                tasks.create_new_time_instance_iris()

        # Trigger derivation update for each time step to optimise
        for run in range(params['numberOfTimeSteps']):
            if run == 0:
                # Initialise optimisation/forecast interval
                opti_dt = params['optHorizon']*params['timeDelta_unix']
                t1 = params['start']                
                t2 = t1 + opti_dt
                if new_derivation:
                    print('Instantiating optimisation time etc. pure inputs ...')
                    # Instantiate required time instances to initiate optimisation cascades
                    kg_client.instantiate_time_instant(sim_t, t1, instance_type=OD_SIMULATION_TIME) 
                    kg_client.instantiate_time_interval(opti_int, opti_t1, opti_t2, t1, t2)
                    # Instantiate required durations
                    kg_client.instantiate_time_duration(heat_length, params['timeDelta'], 
                                                        params['heatDemandDataLength'])
                    kg_client.instantiate_time_duration(tmp_length, params['timeDelta'], 
                                                        params['gridTemperatureDataLength'])
                    # Instantiate required frequency for forecasting agent
                    kg_client.instantiate_time_duration(freq, params['timeDelta'], 
                                                        value=1, rdf_type=TS_FREQUENCY)
                    # Add time stamps to pure inputs
                    derivation_client.addTimeInstanceCurrentTimestamp(
                        [sim_t, opti_int, heat_length, tmp_length, freq])
                    print('Instantiation of optimisation time etc. successfully finished.')
                else:
                    # Update time instances (pre-existing from previous optimisation)
                    print('Updating time stamps of pure inputs ...')
                    tasks.update_time_instances(kg_client, derivation_client, opti_int,
                                                sim_t, t1, opti_t1, t1, opti_t2, t2)
                
                ###   Instantiate derivation markups   ###
                # 1) Forecast derivations
                print('Instantiate/update forecast derivations ...')
                #    1) Heat demand
                if not fc_deriv_iris:
                    heat_demand = kg_client.get_heat_demand()
                    inputs_demand = [heat_demand, fc_model_heat_demand, opti_int, freq, heat_length]
                    # NOTE: Forecast derivations are instantiated using "createSyncDerivationForNewInfo"
                    #       to ensure that derivation outputs are instantiated, which is not the case
                    #       for sole derivation updates when using derivations with time series
                    deriv = derivation_client.createSyncDerivationForNewInfo(FORECASTING_AGENT_IRI, 
                                    inputs_demand, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                    print(f"Heat demand forecast derivation successfully instantiated: {deriv.getIri()}")
                    # Add to list of all forecast derivation IRIs
                    fc_deriv_iris.append(deriv.getIri())
                    #    2) Grid temperatures
                    grid_temps = kg_client.get_grid_temperatures()
                    deriv_base = [fc_model_grid_temperature, opti_int, freq, tmp_length]
                    inputs_temps = [deriv_base + [t] for t in grid_temps]
                    for i in inputs_temps:
                        deriv = derivation_client.createSyncDerivationForNewInfo(FORECASTING_AGENT_IRI, 
                                        i, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                        print(f"Grid temperature forecast derivation successfully instantiated: {deriv.getIri()}")
                        fc_deriv_iris.append(deriv.getIri())
                else:
                    for d in fc_deriv_iris:
                        derivation_client.unifiedUpdateDerivation(d)
                        print(f"Forecast derivation instance successfully updated: {d}")

                # 2) Generation optimisation derivation
                print('Instantiate/update heat generation optimisation derivation ...')
                if not opti_deriv_iri:
                    # NOTE: Instantiated using "createSyncDerivationForNewInfo" for same
                    #       reason as above, i.e., ensure initial generation of output triples
                    # Get all forecast derivation outputs
                    fc_outputs = kg_client.get_derivation_outputs(fc_deriv_iris)
                    # Extract all created forecast instances and create list of optimisation inputs
                    inputs_opti = list(fc_outputs[TS_FORECAST]) + [opti_int]
                    deriv = derivation_client.createSyncDerivationForNewInfo(DH_OPTIMISATION_AGENT_IRI, 
                                    inputs_opti, ONTODERIVATION_DERIVATIONWITHTIMESERIES)
                    opti_deriv_iri.append(deriv.getIri())
                    print(f"Generation optimisation derivation successfully instantiated: {opti_deriv_iri[0]}")
                else:
                    derivation_client.unifiedUpdateDerivation(opti_deriv_iri[0])
                    print(f"Generation optimisation derivation instance successfully updated: {opti_deriv_iri[0]}")

                # 3) Emission estimation derivations
                print('Instantiate/update emission estimation derivations ...')
                if not em_deriv_iri:
                    # Get all optimisation derivation outputs
                    opti_outputs = kg_client.get_derivation_outputs(opti_deriv_iri)
                    # Extract all created forecast instances and create list of optimisation inputs
                    # NOTE: Point source instances associated with emissions need to be specified
                    #       in static_point_sources.ttl and referenced in iris.py beforehand
                    #    1) Waste incineration emissions (ProvidedHeatAmount)
                    inputs_efw_em = list(opti_outputs[OHN_PROVIDED_HEAT_AMOUNT]) + [sim_t, point_source_efw]
                    deriv = derivation_client.createSyncDerivationForNewInfo(EMISSION_ESTIMATION_AGENT_IRI, 
                                    inputs_efw_em, ONTODERIVATION_DERIVATION)
                    print(f"EfW emission estimation derivation successfully instantiated: {deriv.getIri()}")
                    #    2) Heating plant emissions (ConsumedGasAmount)
                    inputs_mu_em = list(opti_outputs[OHN_CONSUMED_GAS_AMOUNT]) + [sim_t, point_source_mu]
                    deriv = derivation_client.createSyncDerivationForNewInfo(EMISSION_ESTIMATION_AGENT_IRI, 
                                    inputs_mu_em, ONTODERIVATION_DERIVATION)
                    print(f"Municipal utility emission estimation derivation successfully instantiated: {deriv.getIri()}")
                else:
                    for d in em_deriv_iri:
                        derivation_client.unifiedUpdateDerivation(d)
                        print(f"Emission estimation derivation instance successfully updated: {d}")

                # 4) Initialise Aermod dispersion derivation markup (for existing
                #    SimulationTime instance) by sending POST request to Dispersion
                #    Interactor agent
                # NOTE: Aermod architecture allows for initialisation/requesting
                #       of multiple derivations at the same time
                # Initialise dict of all dispersion derivations
                disp_deriv_iris = {}
                pathlist = list(Path("./resources/dispersion_interactor").glob('*.json'))
                for p in pathlist:
                    # Read "hardcoded" parameters from json files (i.e., in bind-mount)
                    with open(p, "r") as file:
                        logger.info(f'Loading dispersion derivation file: {p}')
                        parameters = json.load(file)
                    parameters['simulationTimeIri'] = sim_t
                    # Send POST request incl. pre-existing Simulation Time IRI
                    # NOTE: Returns derivation IRI of 1) newly created derivation or
                    #       2) already instantiated derivation (i.e., to be updated)
                    response = requests.post(DISPERSION_INTERACTOR_URL, data=parameters)
                    if response.status_code == 200:
                        try:
                            # Parse the response JSON and extract "derivation" IRI
                            result = response.json()
                            disp_deriv_iri = result.get("derivation")
                            disp_deriv_iris[parameters.get('label')] = disp_deriv_iri
                            print(f"Dispersion derivation instance with label \"{parameters.get('label')}\" "
                                  +f"(created or retrieved): {disp_deriv_iri}")
                        except:
                            raise ValueError(f"Dispersion derivation with label \"{parameters.get('label')}\" : "
                                            +f"Creation/retrieval request failed with message: {response.text}. "
                                            +"Ensure that Ontop endpoint is running.")
                    else:
                        raise ValueError(f"Dispersion derivation with label \"{parameters.get('label')}\" : "
                                        +f"Creation/retrieval request failed with message: {response.text}")

            else:
                t1 += params['timeDelta_unix']
                t2 += params['timeDelta_unix']
                # Update required time instances to trigger next optimisation run
                print('Updating time stamps of pure inputs ...')
                tasks.update_time_instances(kg_client, derivation_client, opti_int,
                                            sim_t, t1, opti_t1, t1, opti_t2, t2)


            # Request derivation update(s) via Aermod Agent
            print('')
            print(f"Trigger optimisation run {run+1}/{params['numberOfTimeSteps']} "
                  +"by requesting derivation update(s) from Aermod ...")
            for key, value in disp_deriv_iris.items():
                logger.info(f'Requesting update for dispersion derivation \"{key}\".')
                derivation_client.unifiedUpdateDerivation(value)
            # NOTE: Aermod Agent queries emission derivations via StaticPointSources 
            # and requests update; all other derivation updates are handled by DIF 
            # directly as derivationscare directly linked via I/O relations in KG            

            # Print progress (to ensure output to console even for async tasks)
            print(f"Optimisation run for time {t1} completed.\n")

        print("Optimisation completed successfully.")

    except Exception as ex:
        # Log the exception
        logger.error("An error occurred during optimisation.", exc_info=True)
        raise RuntimeError("An error occurred during optimisation.") from ex


def is_processing_task_running():
    # Return True if any 'perform_optimisation_task' is currently running
    inspect = celery.control.inspect()
    active_tasks = inspect.active()
    if any(active_tasks.values()):
        active_task_names = [v[0].get('name') for v in active_tasks.values()]
        return any('trigger_optimisation_task' in item for item in active_task_names)
    return False


if __name__ == "__main__":
    # Start the app
    app.run(host='localhost', port="5000")
    logger.info('App started')