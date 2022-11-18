from testcontainers.core.container import DockerContainer
from testcontainers.compose import DockerCompose
from pathlib import Path
from flask import Flask
import requests
import logging
import pkgutil
import pytest
import shutil
import time
import uuid
# import xlwt
import os

from pyderivationagent.conf import config_derivation_agent
from pyderivationagent.kg_operations import PySparqlClient
from chemistry_and_robots.kg_operations import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.kg_operations.dict_and_list as dal
from chemistry_and_robots.data_model import *

from doeagent.agent import DoEAgent
from vapourtecscheduleagent.agent import VapourtecScheduleAgent
from hplcpostproagent.agent import HPLCPostProAgent
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.conf import config_vapourtec_agent
from hplcagent.agent import HPLCAgent
from hplcagent.conf import config_hplc_agent

# RxnOptGoal Agent related imports
from rxnoptgoalagent.conf.rxn_opt_goal_agent_conf import config_rxn_opt_goal_agent
from rxnoptgoalagent.agent import RxnOptGoalAgent
from rxnoptgoalagent.data_model import *

try:
    import rxnoptgoaliteragent.tests.conftest as rogi_cf
except ImportError:
    raise ImportError("""If rxnoptgoaliteragent is not installed, then please install it first.
                      Otherwise, it's possible due to tests and tests.* are not packaged in the rxnoptgoaliteragent package.
                      One way to fix this is to change the following line in RxnOptGoalIterAgent/setup.py:
                      ``packages=find_packages(exclude=['tests','tests.*']),``
                      to: ``packages=find_packages(),``.
                      Then reinstall the dev version of rxnoptgoaliteragent via:
                      ``cd <path_to_twa_agents>/Agents/RxnOptGoalIterAgent && python -m pip install -e .``
                      A better way of designing tests across multiple agents will be provided in the future.""")

from rxnoptgoaliteragent.agent import RxnOptGoalIterAgent

logging.getLogger("py4j").setLevel(logging.INFO)
logging.getLogger("numba").setLevel(logging.WARNING)


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR,'env_files')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
TEST_TRIPLES_DIR = os.path.join(THIS_DIR,'test_triples')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'_downloaded_files_for_test')
HPLC_REPORT_LOCAL_TEST_DIR = os.path.join(THIS_DIR,'_generated_hplc_report_for_test')
FCEXP_FILE_DIR = os.path.join(THIS_DIR,'_generated_vapourtec_input_file_for_test')
DOCKER_INTEGRATION_DIR = os.path.join(THIS_DIR,'_for_docker_integration_test')
HTML_TEMPLATE_DIR = os.path.join(os.path.dirname(THIS_DIR), 'templates')
EMAIL_AUTH_JSON_PATH = os.path.join(SECRETS_PATH, 'email_auth.json')

# Raw HPLC report sample data in the test_triples folder
HPLC_REPORT_XLS_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_xls.xls')
HPLC_REPORT_TXT_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_txt.txt')


KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
KG_EXPOSED_PORT = 8080 # specified in docker-compose.test.kg.yml
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"
FS_EXPOSED_PORT = 8080 # specified in docker-compose.test.kg.yml
DOCKER_COMPOSE_TEST_KG = 'docker-compose.test.kg.yml'

# Configuration env files
# NOTE the triple store and file server URL ("localhost") provided in the agent.*.env files are made possible via:
# "extra_hosts: - localhost:host-gateway" in the docker-compose.test.yml
DOE_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.doe.env.test')
VAPOURTEC_SCHEDULE_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.schedule.env.test')
HPLC_POSTPRO_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.postpro.env.test')
# VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.env.test')
# HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.env.test')
ROG_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.env.test')
ROGI_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.iter.env.test')

LAB1_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.vapourtec.env.test')
LAB1_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.hplc.env.test')
LAB2_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.vapourtec.env.test')
LAB2_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.hplc.env.test')

DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(DOE_AGENT_ENV).DERIVATION_INSTANCE_BASE_URL

DOE_IRI = 'http://example.com/blazegraph/namespace/testlab/doe/DoE_1'
DERIVATION_INPUTS = [DOE_IRI]

def create_rog_agent(
    goal_iter_agent_iri:str=None,
    kg_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
    kg_update_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
    # NOTE: it is assumed that the KG test container doesn't has authentication
    # also, we assume that we don't need file server for testing with the KG test container
):
    rog_agent_config = config_rxn_opt_goal_agent(ROG_AGENT_ENV)
    rog_agent = RxnOptGoalAgent(
        goal_agent_iri=rog_agent_config.GOAL_ONTOAGENT_SERVICE_IRI,
        goal_agent_endpoint=rog_agent_config.GOAL_ONTOAGENT_OPERATION_HTTP_URL,
        goal_monitor_time_interval=rog_agent_config.GOAL_MONITOR_PERIODIC_TIMESCALE,
        goal_iter_agent_iri=rog_agent_config.GOAL_ITER_AGENT_IRI if not goal_iter_agent_iri else goal_iter_agent_iri,
        derivation_instance_base_url=rog_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=rog_agent_config.SPARQL_QUERY_ENDPOINT if not kg_url else kg_url,
        kg_update_url=rog_agent_config.SPARQL_UPDATE_ENDPOINT if not kg_update_url else kg_update_url,
        kg_user=rog_agent_config.KG_USERNAME if not kg_url else None,
        kg_password=rog_agent_config.KG_PASSWORD if not kg_url else None,
        fs_url=rog_agent_config.FILE_SERVER_ENDPOINT if not kg_url else None,
        fs_user=rog_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
        fs_password=rog_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
        app=Flask(__name__, template_folder=HTML_TEMPLATE_DIR)
    )
    return rog_agent

def create_rogi_agent(
    register_agent:bool=False,
    random_agent_iri:bool=False,
    kg_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
    kg_update_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
    # NOTE: it is assumed that the KG test container doesn't has authentication
    # also, we assume that we don't need file server for testing with the KG test container
):
    rogi_agent_config = config_derivation_agent(ROGI_AGENT_ENV)
    rogi_agent = RxnOptGoalIterAgent(
        register_agent=rogi_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=rogi_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=rogi_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=rogi_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=rogi_agent_config.SPARQL_QUERY_ENDPOINT if not kg_url else kg_url,
        kg_update_url=rogi_agent_config.SPARQL_UPDATE_ENDPOINT if not kg_update_url else kg_update_url,
        kg_user=rogi_agent_config.KG_USERNAME if not kg_url else None,
        kg_password=rogi_agent_config.KG_PASSWORD if not kg_url else None,
        fs_url=rogi_agent_config.FILE_SERVER_ENDPOINT if not kg_url else None,
        fs_user=rogi_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
        fs_password=rogi_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
        agent_endpoint=rogi_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        email_recipient=rogi_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=rogi_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=rogi_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=rogi_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return rogi_agent

def create_doe_agent(
    register_agent:bool=False,
    random_agent_iri:bool=False,
):
    doe_agent_config = config_derivation_agent(DOE_AGENT_ENV)
    doe_agent = DoEAgent(
        register_agent=doe_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=doe_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=doe_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=doe_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=doe_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=doe_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=doe_agent_config.KG_USERNAME,
        kg_password=doe_agent_config.KG_PASSWORD,
        fs_url=doe_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=doe_agent_config.FILE_SERVER_USERNAME,
        fs_password=doe_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=doe_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        email_recipient=doe_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=doe_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=doe_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=doe_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return doe_agent

def create_vapourtec_schedule_agent(
    maximum_concurrent_experiment:int=None,
    register_agent:bool=False,
    random_agent_iri:bool=False,
    derivation_periodic_timescale:int=None,
):
    vapourtec_schedule_agent_config = config_derivation_agent(VAPOURTEC_SCHEDULE_AGENT_ENV)
    vapourtec_schedule_agent = VapourtecScheduleAgent(
        register_agent=vapourtec_schedule_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=vapourtec_schedule_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=vapourtec_schedule_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
        derivation_instance_base_url=vapourtec_schedule_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=vapourtec_schedule_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=vapourtec_schedule_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=vapourtec_schedule_agent_config.KG_USERNAME,
        kg_password=vapourtec_schedule_agent_config.KG_PASSWORD,
        fs_url=vapourtec_schedule_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=vapourtec_schedule_agent_config.FILE_SERVER_USERNAME,
        fs_password=vapourtec_schedule_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=vapourtec_schedule_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        max_thread_monitor_async_derivations=vapourtec_schedule_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS if maximum_concurrent_experiment is None else maximum_concurrent_experiment,
        email_recipient=vapourtec_schedule_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=vapourtec_schedule_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=vapourtec_schedule_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=vapourtec_schedule_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return vapourtec_schedule_agent

def create_hplc_postpro_agent(
    register_agent:bool=False,
    random_agent_iri:bool=False,
):
    hplc_postpro_agent_config = config_derivation_agent(HPLC_POSTPRO_AGENT_ENV)
    hplc_postpro_agent = HPLCPostProAgent(
        register_agent=hplc_postpro_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=hplc_postpro_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=hplc_postpro_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=hplc_postpro_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=hplc_postpro_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=hplc_postpro_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=hplc_postpro_agent_config.KG_USERNAME,
        kg_password=hplc_postpro_agent_config.KG_PASSWORD,
        fs_url=hplc_postpro_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=hplc_postpro_agent_config.FILE_SERVER_USERNAME,
        fs_password=hplc_postpro_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=hplc_postpro_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        email_recipient=hplc_postpro_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=hplc_postpro_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=hplc_postpro_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=hplc_postpro_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return hplc_postpro_agent

def create_vapourtec_agent(
    env_file:str,
    vapourtec_digital_twin:str=None,
    vapourtec_state_periodic_timescale:int=None,
    fcexp_file_container_folder:str=None,
    fcexp_template_filename:str="JB_Experiment.fcexp",
    register_agent:bool=False,
    random_agent_iri:bool=False,
    derivation_periodic_timescale:int=None,
    dry_run: bool = True,
):
    vapourtec_agent_config = config_vapourtec_agent(env_file)
    vapourtec_agent = VapourtecAgent(
        vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN if vapourtec_digital_twin is None else vapourtec_digital_twin,
        vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE if vapourtec_state_periodic_timescale is None else vapourtec_state_periodic_timescale,
        vapourtec_ip_address=vapourtec_agent_config.VAPOURTEC_IP_ADDRESS,
        fcexp_file_container_folder=vapourtec_agent_config.FCEXP_FILE_CONTAINER_FOLDER if fcexp_file_container_folder is None else fcexp_file_container_folder,
        fcexp_template_filename=fcexp_template_filename,
        dry_run=dry_run, # NOTE
        register_agent=vapourtec_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=vapourtec_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=vapourtec_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
        derivation_instance_base_url=vapourtec_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=vapourtec_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=vapourtec_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=vapourtec_agent_config.KG_USERNAME,
        kg_password=vapourtec_agent_config.KG_PASSWORD,
        fs_url=vapourtec_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=vapourtec_agent_config.FILE_SERVER_USERNAME,
        fs_password=vapourtec_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=vapourtec_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        email_recipient=vapourtec_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=vapourtec_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=vapourtec_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=vapourtec_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return vapourtec_agent

def create_hplc_agent(
    env_file:str,
    hplc_digital_twin:str=None,
    hplc_report_periodic_timescale:int=None,
    hplc_report_container_dir:str=None,
    hplc_report_file_extension:str=None,
    register_agent:bool=False,
    random_agent_iri:bool=False,
    derivation_periodic_timescale:int=None,
    dry_run: bool=True,
):
    hplc_agent_config = config_hplc_agent(env_file)
    hplc_agent = HPLCAgent(
        hplc_digital_twin=hplc_agent_config.HPLC_DIGITAL_TWIN if hplc_digital_twin is None else hplc_digital_twin,
        hplc_report_periodic_timescale=hplc_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE if hplc_report_periodic_timescale is None else hplc_report_periodic_timescale,
        hplc_report_container_dir=hplc_agent_config.HPLC_REPORT_CONTAINER_DIR if hplc_report_container_dir is None else hplc_report_container_dir,
        current_hplc_method=hplc_agent_config.CURRENT_HPLC_METHOD,
        hplc_report_file_extension=hplc_agent_config.HPLC_REPORT_FILE_EXTENSION if hplc_report_file_extension is None else hplc_report_file_extension,
        dry_run=dry_run, # NOTE True for testing, False for production
        register_agent=hplc_agent_config.REGISTER_AGENT if not register_agent else register_agent,
        agent_iri=hplc_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
        time_interval=hplc_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
        derivation_instance_base_url=hplc_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=hplc_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=hplc_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=hplc_agent_config.KG_USERNAME,
        kg_password=hplc_agent_config.KG_PASSWORD,
        fs_url=hplc_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=hplc_agent_config.FILE_SERVER_USERNAME,
        fs_password=hplc_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=hplc_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        app=Flask(__name__),
        email_recipient=hplc_agent_config.EMAIL_RECIPIENT,
        email_subject_prefix=hplc_agent_config.EMAIL_SUBJECT_PREFIX,
        email_username=hplc_agent_config.EMAIL_USERNAME,
        email_auth_json_path=EMAIL_AUTH_JSON_PATH,
        email_start_end_async_derivations=hplc_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
    )
    return hplc_agent

# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])


def initialise_triples(sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Upload the example triples for testing
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        if 'chem_rxn' in str(path) or 'lab2' in str(path) or 'doe_template' in str(path):
            g = Graph()
            g.parse(str(path), format='turtle')
            sparql_client.uploadGraph(g)

    # Upload all relevant example triples provided in the test_triples folder of 'rxnoptgoaliteragent' package to triple store
    for f in ['goal_iter.ttl', 'plan_step_agent.ttl']:
        data = pkgutil.get_data('rxnoptgoaliteragent', 'tests/test_triples/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        sparql_client.uploadGraph(g)

# ----------------------------------------------------------------------------------
# Sample data
# ----------------------------------------------------------------------------------
sample_goal_request = {
    "chem_rxn": "https://www.example.com/triplestore/testlab/chem_rxn/ChemRxn_1",
    "cycleAllowance": 30,
    "deadline": "2022-10-30T17:05",
    "first_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#Yield",
    "first_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresGreaterThan",
    "first_goal_num_val": 99,
    "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
    "rxn_opt_goal_plan": "http://www.theworldavatar.com/resource/plans/RxnOpt/rxnoptplan",
    "second_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#RunMaterialCost",
    "second_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresLessThan",
    "second_goal_num_val": 0.001,
    "second_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/poundSterlingPerKilogram",
    "labs": ['http://example.com/blazegraph/namespace/testlab/lab2/Laboratory_Dummy']
}

# @pytest.mark.parametrize(
#     "goal_set_iri,derivation_inputs,hplc_report_target_folder,fcexp_file_container_folder,local_agent_test",
#     [
#         (cf.rogi_cf.IRIs.GOALSET_1.value, cf.rogi_cf.IRIs.DERIVATION_INPUTS.value, cf.HPLC_REPORT_LOCAL_TEST_DIR, cf.FCEXP_FILE_DIR, True),
#         # (cf.rogi_cf.IRIs.GOALSET_1.value, cf.rogi_cf.IRIs.DERIVATION_INPUTS.value, cf.DOCKER_INTEGRATION_DIR, None, False),
#     ],
# )
def _test_rxn_rogi(hplc_report_container_dir, clean_and_initialise_triples: bool = False):

    bg_url = 'http://localhost:48082/blazegraph/namespace/kb/sparql'
    kg_user = 'bg_user'
    kg_pwd = 'test_password'
    fs_url = 'http://localhost:48086/FileServer/'
    fs_user = 'fs_user'
    fs_pwd = 'test_password'

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(
        query_endpoint=bg_url,
        update_endpoint=bg_url,
        kg_user=kg_user,
        kg_password=kg_pwd,
        fs_url=fs_url,
        fs_user=fs_user,
        fs_pwd=fs_pwd,
    )

    if clean_and_initialise_triples:
        # Clear triple store before any usage
        sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

        initialise_triples(sparql_client)

    # Create DerivationClient for creating derivation instances
    derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        sparql_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    vapourtec_agent = create_vapourtec_agent(
        env_file=LAB2_VAPOURTEC_AGENT_ENV,
        register_agent=True,
        fcexp_file_container_folder=DOCKER_INTEGRATION_DIR,
        dry_run=False,
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=LAB2_HPLC_AGENT_ENV,
        register_agent=True,
        hplc_report_container_dir=hplc_report_container_dir,
        dry_run=False,
    )

    # Start the scheduler to monitor derivations if it's local agent test
    rogi_agent.start_all_periodical_job()
    doe_agent.start_all_periodical_job()
    vapourtec_schedule_agent.start_all_periodical_job()
    hplc_postpro_agent.start_all_periodical_job()
    vapourtec_agent.start_all_periodical_job()
    hplc_agent.start_all_periodical_job()

    rog_agent_endpoint = "http://localhost:5000/goal_specification"
    x = requests.post(rog_agent_endpoint, data = sample_goal_request)
    time.sleep(3600000)

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    rogi_agent.scheduler.shutdown()
    doe_agent.scheduler.shutdown()
    vapourtec_schedule_agent.scheduler.shutdown()
    hplc_postpro_agent.scheduler.shutdown()
    vapourtec_agent.scheduler.shutdown()
    hplc_agent.scheduler.shutdown()

if __name__ == '__main__':
    # TODO revise this to use pytest
    hplc_report_container_dir = "C:\\Chem32\\1\\Data\\Optimization\\Optimization 2022-09-19 22-41-02"
    _test_rxn_rogi(hplc_report_container_dir, clean_and_initialise_triples=False)
