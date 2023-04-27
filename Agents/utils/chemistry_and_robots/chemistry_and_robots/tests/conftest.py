from testcontainers.core.container import DockerContainer
from testcontainers.compose import DockerCompose
from pathlib import Path
from rdflib import Graph
from enum import Enum
import pytest
import shutil
import time
import uuid
import os

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.data_model as onto

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
RESOURCE_DIR = os.path.join(str(Path(__file__).absolute().parent.parent),'resources')
SAMPLE_DATA_DIR = os.path.join(RESOURCE_DIR,'sample_data')
SPACE_DASH_DOT_DIR = os.path.join(SAMPLE_DATA_DIR, 'space - with.dir')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')

HPLC_XLS_REPORT_FILE = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_xls.xls')
HPLC_TXT_REPORT_FILE = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_txt.txt')
HPLC_XLS_REPORT_FILE_INCOMPLETE = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_xls_incomplete.xls')
HPLC_TXT_REPORT_FILE_INCOMPLETE = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_txt_incomplete.txt')
HPLC_XLS_REPORT_FILE_UNIDENTIFIED_PEAKS = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_xls_unidentified_peaks.xls')
HPLC_TXT_REPORT_FILE_UNIDENTIFIED_PEAKS = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_txt_unidentified_peaks.txt')
HPLC_XLS_REPORT_FILE_NO_PRODUCT = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_xls_no_product.xls')
HPLC_TXT_REPORT_FILE_NO_PRODUCT = os.path.join(SAMPLE_DATA_DIR,'raw_hplc_report_txt_no_product.txt')
HPLC_TXT_REPORT_FILE_WITH_SPACE = os.path.join(SPACE_DASH_DOT_DIR,'raw_hplc_report_txt with space.txt')
VAPOURTEC_INPUT_FILE = os.path.join(SAMPLE_DATA_DIR,'vapourtec_input.csv')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
KG_EXPOSED_PORT = 8080 # specified in docker-compose.yml
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"
FS_EXPOSED_PORT = 8080 # specified in docker-compose.yml
DOCKER_COMPOSE_TEST_KG = 'docker-compose.yml'

def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# ----------------------------------------------------------------------------------

# @pytest.fixture(scope="session")
# def get_service_url(session_scoped_container_getter):
#     def _get_service_url(service_name, url_route):
#         service = session_scoped_container_getter.get(service_name).network_info[0]
#         service_url = f"http://localhost:{service.host_port}/{url_route}"
#         return service_url

#     # this will run only once per entire test session and ensures that all the services
#     # in docker containers are ready. Increase the sleep value in case services need a bit
#     # more time to run on your machine.
#     time.sleep(8)
#     return _get_service_url

@pytest.fixture(scope="session")
def get_service_auth():
    def _get_service_auth(service_name):
        password_file = os.path.join(SECRETS_PATH,service_name+'_passwd.txt')
        user_file = os.path.join(SECRETS_PATH,service_name+'_user.txt')

        # read service auth from files
        username = ''
        password = ''
        if os.path.exists(user_file):
            with open(user_file) as f:
                username = f.read().strip()
        if os.path.exists(password_file):
            with open(password_file) as f:
                password = f.read().strip()

        return username, password

    return _get_service_auth

@pytest.fixture(scope="session")
def generate_random_download_path():
    def _generate_random_download_path(filename_extension):
        return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)
    return _generate_random_download_path

# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

# NOTE this fixture uses DockerCompose from the testcontainers library to start the docker
# containers. The docker-compose.yml file is in the directory of this file
@pytest.fixture(scope="module")
def initialise_blazegraph_and_fileserver():
    docker_compose = DockerCompose(THIS_DIR, DOCKER_COMPOSE_TEST_KG, pull=True)
    with docker_compose as containers:
        bg_host = containers.get_service_host(KG_SERVICE, KG_EXPOSED_PORT)
        bg_port = containers.get_service_port(KG_SERVICE, KG_EXPOSED_PORT)
        fs_host = containers.get_service_host(FS_SERVICE, FS_EXPOSED_PORT)
        fs_port = containers.get_service_port(FS_SERVICE, FS_EXPOSED_PORT)
        bg_url = f'http://{bg_host}:{bg_port}/{KG_ROUTE}'
        fs_url = f'http://{fs_host}:{fs_port}/{FS_ROUTE}'
        yield bg_url, fs_url

# NOTE the scope is set as "module" so that the operations in test_hplc.py does NOT affect those in test_sparql_client.py
@pytest.fixture(scope="module")
def initialise_triples(initialise_blazegraph_and_fileserver, get_service_auth):
    # Retrieve endpoint for triple store and file server
    sparql_endpoint, fs_url = initialise_blazegraph_and_fileserver

    # Retrieve auth for triple store and file server
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)
    fs_user, fs_pwd = get_service_auth(FS_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd,
        fs_url=fs_url, fs_user=fs_user, fs_pwd=fs_pwd
    )

    # Upload the example triples for testing
    pathlist = Path(RESOURCE_DIR).glob('**/*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)

    sparql_client.upload_ontology_tbox(onto.ONTODOE)
    sparql_client.upload_ontology_tbox(onto.ONTOREACTION)

    # Create folder for downloaded files
    if not os.path.exists(DOWNLOADED_DIR):
        os.mkdir(DOWNLOADED_DIR)

    yield sparql_client

    # Clear all triples
    # NOTE so that the sparql operations will not effect each other between different modules
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Clear logger at the end of the test
    clear_loggers()

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)

class TargetIRIs(Enum):
    CHEMICAL_REACTION_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ChemRxn_1/'
    DUMMY_VAPOURTEC_EXECUTION_AGENT_SERVICE_IRI = 'https://www.theworldavatar.com/kg/agents/Service__Execution/Service' # same as in rxn_queue_test.ttl
    DUMMY_LAB_BASE_IRI = 'https://example.com/blazegraph/namespace/testlab/dummy_lab/'
    DUMMY_LAB_IRI = DUMMY_LAB_BASE_IRI + 'Laboratory_Dummy'
    AUTOMATEDRXNPLATFORM_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'AutomatedRxnPlatform_Dummy'
    VAPOURTECRS400_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecRS400_Dummy'
    VAPOURTEC_LTD = DUMMY_LAB_BASE_IRI + 'VapourtecLtd'
    VAPOURTECRS400_POWERSUPPLY = DUMMY_LAB_BASE_IRI + 'VapRS400_PowerSupply'
    VAPOURTECRS400_DUMMY_COLLECTION_METHOD_IRI = DUMMY_LAB_BASE_IRI + 'CollectionMethod_VapourtecRS400_Dummy'
    VAPOURTECRS400_DUMMY_COLLECTION_METHOD_TO_RECEPTACLE_IRI = DUMMY_LAB_BASE_IRI + 'WasteReceptacle_Dummy'
    HPLC_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLC_Dummy'
    HPLC_DUMMY_MANUFACTURER_IRI = DUMMY_LAB_BASE_IRI + 'HPLC_Manufacturer'
    BPR_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'BPR_Dummy'
    VAPOURTECR4REACTOR_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Dummy'
    VAPOURTECR4REACTOR_DUMMY_VOLUME = 10
    VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Another_Dummy'
    VAPOURTECR4REACTOR_ANOTHER_DUMMY_VOLUME = 10
    VAPOURTECR2PUMP_1_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_1_Dummy'
    VAPOURTECR2PUMP_2_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_2_Dummy'
    VAPOURTECR2PUMP_3_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_3_Dummy'
    VAPOURTECR2PUMP_4_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR2_4_Dummy'
    REAGENTBOTTLE_1_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'ReagentBottle_1_Dummy'
    AUTOSAMPLER_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'AutoSampler_Dummy'
    AUTOSAMPLER_SAMPLE_LOOP_VOLUME_IRI = DUMMY_LAB_BASE_IRI + 'AutoSampler_Dummy_SampleLoopVolume'
    AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_IRI = DUMMY_LAB_BASE_IRI + 'AutoSampler_Dummy_SampleLoopVolume_value'
    AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_UNIT = onto.OM_MILLILITRE
    AUTOSAMPLER_SAMPLE_LOOP_VOLUME_MEASURE_NUM_VAL = 2.0
    AUTOSAMPLER_LIQUID_LEVEL_DICT = {DUMMY_LAB_BASE_IRI + 'Site_56': 0.0,
        DUMMY_LAB_BASE_IRI + 'Site_153': 0.0,
        DUMMY_LAB_BASE_IRI + 'Site_3': 7.0,
        DUMMY_LAB_BASE_IRI + 'Site_2': 7.0,
        DUMMY_LAB_BASE_IRI + 'Site_1': 5.0,
        DUMMY_LAB_BASE_IRI + 'Site_4': 7.0,
        DUMMY_LAB_BASE_IRI + 'Site_216': 0.0,
        DUMMY_LAB_BASE_IRI + 'Site_5': 3.0}
    AUTOSAMPLER_LIQUID_COMPONENT_DICT = {
        DUMMY_LAB_BASE_IRI + 'Site_4': ['http://www.theworldavatar.com/kg/ontospecies/Species_f999de28-55dc-477e-8afc-e8802064e0d2',
            'http://www.theworldavatar.com/kg/ontospecies/Species_8765d201-0da9-4112-b653-3455002f535b'],
        DUMMY_LAB_BASE_IRI + 'Site_5': ['http://www.theworldavatar.com/kg/ontospecies/Species_f999de28-55dc-477e-8afc-e8802064e0d2',
            'http://www.theworldavatar.com/kg/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340'],
        DUMMY_LAB_BASE_IRI + 'Site_3': ['http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202',
            'http://www.theworldavatar.com/kg/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983'],
        DUMMY_LAB_BASE_IRI + 'Site_1': ['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
            'http://www.theworldavatar.com/kg/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb',
            'http://www.theworldavatar.com/kg/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340'],
        DUMMY_LAB_BASE_IRI + 'Site_2': ['http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
            'http://www.theworldavatar.com/kg/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb']
    }
    AUTOSAMPLER_LIQUID_CONTAINS_UNIDENTIFIED_COMPONENT_DICT = {
        DUMMY_LAB_BASE_IRI + 'Site_4': False,
        DUMMY_LAB_BASE_IRI + 'Site_5': True,
        DUMMY_LAB_BASE_IRI + 'Site_3': False,
        DUMMY_LAB_BASE_IRI + 'Site_1': False,
        DUMMY_LAB_BASE_IRI + 'Site_2': False,
    }
    AUTOSAMPLER_LIQUID_LEVEL_UNIT_DICT = {
        DUMMY_LAB_BASE_IRI + 'Site_153': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_4': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_56': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_5': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_3': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_1': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_216': onto.OM_MILLILITRE,
        DUMMY_LAB_BASE_IRI + 'Site_2': onto.OM_MILLILITRE
    }
    AUTOSAMPLER_SITE_LOC_DICT = {
        DUMMY_LAB_BASE_IRI + 'Site_153': '153',
        DUMMY_LAB_BASE_IRI + 'Site_4': '4',
        DUMMY_LAB_BASE_IRI + 'Site_56': '56',
        DUMMY_LAB_BASE_IRI + 'Site_5': '5',
        DUMMY_LAB_BASE_IRI + 'Site_3': '3',
        DUMMY_LAB_BASE_IRI + 'Site_1': '1',
        DUMMY_LAB_BASE_IRI + 'Site_216': '216',
        DUMMY_LAB_BASE_IRI + 'Site_2': '2'
    }
    VAPOURTECRS400_DUMMY_CONSISTS_OF_LIST = [VAPOURTECR4REACTOR_DUMMY_IRI,
        VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI, VAPOURTECR2PUMP_1_DUMMY_IRI,
        VAPOURTECR2PUMP_2_DUMMY_IRI, VAPOURTECR2PUMP_3_DUMMY_IRI,
        VAPOURTECR2PUMP_4_DUMMY_IRI, AUTOSAMPLER_DUMMY_IRI]
    EXP_1_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/'
    EXP_2_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_2/'
    EXP_3_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_3/'
    EXP_4_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_4/'
    EXP_5_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_5/'
    EXAMPLE_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_2_IRI = EXP_2_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_3_IRI = EXP_3_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_4_IRI = EXP_4_BASE_IRI + 'RxnExp_1'
    EXAMPLE_RXN_EXP_5_IRI = EXP_5_BASE_IRI + 'RxnExp_1'
    NEW_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
    NEW_RXN_EXP_2_IRI = EXP_1_BASE_IRI + 'ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
    NEW_RXN_EXP_3_IRI = EXP_1_BASE_IRI + 'ReactionVariation_c4b175d9-e53c-4d7e-b053-3a81f7ca0ddf'
    EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_1',EXP_1_BASE_IRI+'RxnTemperature_1',EXP_1_BASE_IRI+'RxnPressure_1',
        EXP_1_BASE_IRI+'StoiRatio_1',EXP_1_BASE_IRI+'StoiRatio_2',EXP_1_BASE_IRI+'StoiRatio_3',EXP_1_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_1_BASE_IRI+'Yield_1',EXP_1_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST = [EXP_2_BASE_IRI+'ResidenceTime_1',EXP_2_BASE_IRI+'RxnTemperature_1',EXP_2_BASE_IRI+'RxnPressure_1',
        EXP_2_BASE_IRI+'StoiRatio_1',EXP_2_BASE_IRI+'StoiRatio_2',EXP_2_BASE_IRI+'StoiRatio_3',EXP_2_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_2_BASE_IRI+'Yield_1',EXP_2_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST = [EXP_3_BASE_IRI+'ResidenceTime_1',EXP_3_BASE_IRI+'RxnTemperature_1',EXP_3_BASE_IRI+'RxnPressure_1',
        EXP_3_BASE_IRI+'StoiRatio_1',EXP_3_BASE_IRI+'StoiRatio_2',EXP_3_BASE_IRI+'StoiRatio_3',EXP_3_BASE_IRI+'RxnScale_1',]
    EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_3_BASE_IRI+'Yield_1',EXP_3_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST = [EXP_4_BASE_IRI+'ResidenceTime_1',EXP_4_BASE_IRI+'RxnTemperature_1',EXP_4_BASE_IRI+'RxnPressure_1',
        EXP_4_BASE_IRI+'StoiRatio_1',EXP_4_BASE_IRI+'StoiRatio_2',EXP_4_BASE_IRI+'StoiRatio_3',EXP_4_BASE_IRI+'RxnScale_1',]
    EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_4_BASE_IRI+'Yield_1',EXP_4_BASE_IRI+'RunMaterialCost_1']
    EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST = [EXP_5_BASE_IRI+'ResidenceTime_1',EXP_5_BASE_IRI+'RxnTemperature_1',EXP_5_BASE_IRI+'RxnPressure_1',
        EXP_5_BASE_IRI+'StoiRatio_1',EXP_5_BASE_IRI+'StoiRatio_2',EXP_5_BASE_IRI+'StoiRatio_3',EXP_5_BASE_IRI+'RxnScale_1']
    EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST = [EXP_5_BASE_IRI+'Yield_1',EXP_5_BASE_IRI+'RunMaterialCost_1']
    NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_f228988f-7b01-4776-84c0-8cf41bb57176',
        EXP_1_BASE_IRI+'ReactionPressure_a75c05d9-e6c5-4710-8618-cfada4d27ad3',
        EXP_1_BASE_IRI+'ReactionScale_12f103bd-5831-4538-80f1-e9f1b67ccf3b',
        EXP_1_BASE_IRI+'ReactionTemperature_ccb93a20-fea9-4179-a3c2-067e31989adf',
        EXP_1_BASE_IRI+'StoichiometryRatio_866bfdf2-0d32-40da-8fcb-f89669cf1d31',
        EXP_1_BASE_IRI+'StoichiometryRatio_8ae63d49-d3e5-469c-a85a-e7799ddf1807',
        EXP_1_BASE_IRI+'StoichiometryRatio_c242fa46-e60c-481c-8dc2-741f69386f25']
    NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_e8c453af-4dca-4e0b-8137-a073e1ab0d82',
        EXP_1_BASE_IRI+'ReactionPressure_06f24390-c46f-4a82-b998-d52f6b03701d',
        EXP_1_BASE_IRI+'ReactionScale_340aac69-106c-4894-94b9-a7c489b5c597',
        EXP_1_BASE_IRI+'ReactionTemperature_9809328d-1e34-4b5f-87aa-67e4accb75bd',
        EXP_1_BASE_IRI+'StoichiometryRatio_4f55a3ff-3a06-4749-8775-52d7673497c7',
        EXP_1_BASE_IRI+'StoichiometryRatio_58691e29-9d69-41ad-ab5f-8fd9b35f5e0c',
        EXP_1_BASE_IRI+'StoichiometryRatio_6a845185-032d-41d8-86ff-6a9952418063']
    NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST = [EXP_1_BASE_IRI+'ResidenceTime_6e59352a-5296-4d13-8df9-2839b1f66eef',
        EXP_1_BASE_IRI+'ReactionPressure_b57d5d10-70bc-43c5-9424-f493aeda7967',
        EXP_1_BASE_IRI+'ReactionScale_2845e69e-1441-4758-9803-f038744cea8a',
        EXP_1_BASE_IRI+'ReactionTemperature_867f1bd9-0c7b-41d1-adc6-44738631a568',
        EXP_1_BASE_IRI+'StoichiometryRatio_3712c062-8b67-4a82-82d9-166ff34909ba',
        EXP_1_BASE_IRI+'StoichiometryRatio_9968472e-74b9-4925-b5cc-63220f4be1a9',
        EXP_1_BASE_IRI+'StoichiometryRatio_fb5fecbb-3de2-4d98-b96c-38a3d033ca2d']
    NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST = None
    NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST = None
    NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST = None
    RXN_EXP_REACTION_CONDITION_CLZ_POSITIONAL_ID_DICT = {EXAMPLE_RXN_EXP_1_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        EXAMPLE_RXN_EXP_2_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        EXAMPLE_RXN_EXP_3_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        EXAMPLE_RXN_EXP_4_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        EXAMPLE_RXN_EXP_5_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        NEW_RXN_EXP_1_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        NEW_RXN_EXP_2_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']},
        NEW_RXN_EXP_3_IRI:{onto.ONTOREACTION_RESIDENCETIME:[None], onto.ONTOREACTION_REACTIONPRESSURE:[None],
        onto.ONTOREACTION_REACTIONSCALE:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b'],
        onto.ONTOREACTION_REACTIONTEMPERATURE:[None], onto.ONTOREACTION_STOICHIOMETRYRATIO:['http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
        'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
        'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202']}}
    RXN_EXP_REACTION_CONDITION_POSITIONAL_ID_DICT = {
        EXAMPLE_RXN_EXP_1_IRI:{EXP_1_BASE_IRI+'StoiRatio_2':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                               EXP_1_BASE_IRI+'StoiRatio_3':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        EXAMPLE_RXN_EXP_2_IRI:{EXP_2_BASE_IRI+'StoiRatio_2':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                               EXP_2_BASE_IRI+'StoiRatio_3':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        EXAMPLE_RXN_EXP_3_IRI:{EXP_3_BASE_IRI+'StoiRatio_2':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                               EXP_3_BASE_IRI+'StoiRatio_3':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        EXAMPLE_RXN_EXP_4_IRI:{EXP_4_BASE_IRI+'StoiRatio_2':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                               EXP_4_BASE_IRI+'StoiRatio_3':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        EXAMPLE_RXN_EXP_5_IRI:{EXP_5_BASE_IRI+'StoiRatio_2':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                               EXP_5_BASE_IRI+'StoiRatio_3':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        NEW_RXN_EXP_1_IRI:{EXP_1_BASE_IRI+'StoichiometryRatio_866bfdf2-0d32-40da-8fcb-f89669cf1d31':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                           EXP_1_BASE_IRI+'StoichiometryRatio_c242fa46-e60c-481c-8dc2-741f69386f25':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        NEW_RXN_EXP_2_IRI:{EXP_1_BASE_IRI+'StoichiometryRatio_6a845185-032d-41d8-86ff-6a9952418063':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                           EXP_1_BASE_IRI+'StoichiometryRatio_4f55a3ff-3a06-4749-8775-52d7673497c7':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'},
        NEW_RXN_EXP_3_IRI:{EXP_1_BASE_IRI+'StoichiometryRatio_fb5fecbb-3de2-4d98-b96c-38a3d033ca2d':'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3',
                           EXP_1_BASE_IRI+'StoichiometryRatio_3712c062-8b67-4a82-82d9-166ff34909ba':'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'}}

    CHEMICAL_REACTION_IRI = CHEMICAL_REACTION_BASE_IRI + 'ChemRxn_1'
    REACTANT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_1': 'http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b',
    CHEMICAL_REACTION_BASE_IRI + 'Species_2': 'http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3'}
    PRODUCT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_3': 'http://www.theworldavatar.com/kg/ontospecies/Species_f999de28-55dc-477e-8afc-e8802064e0d2',
    CHEMICAL_REACTION_BASE_IRI + 'Species_4': 'http://www.theworldavatar.com/kg/ontospecies/Species_8765d201-0da9-4112-b653-3455002f535b'}
    CATALYST_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_5': 'http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202'}
    SOLVENT_SPECIES_DICTIONARY = {CHEMICAL_REACTION_BASE_IRI + 'Species_6': 'http://www.theworldavatar.com/kg/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb',
    CHEMICAL_REACTION_BASE_IRI + 'Species_7': 'http://www.theworldavatar.com/kg/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983'}
    EXP_1_INPUT_CHEMICAL_1_IRI = EXP_1_BASE_IRI + 'InputChemical_1'
    EXP_1_INPUT_CHEMICAL_2_IRI = EXP_1_BASE_IRI + 'InputChemical_2'
    EXP_1_INPUT_CHEMICAL_3_IRI = EXP_1_BASE_IRI + 'InputChemical_3'
    EXP_2_INPUT_CHEMICAL_1_IRI = EXP_2_BASE_IRI + 'InputChemical_1'
    EXP_2_INPUT_CHEMICAL_2_IRI = EXP_2_BASE_IRI + 'InputChemical_2'
    EXP_2_INPUT_CHEMICAL_3_IRI = EXP_2_BASE_IRI + 'InputChemical_3'
    EXP_3_INPUT_CHEMICAL_1_IRI = EXP_3_BASE_IRI + 'InputChemical_1'
    EXP_3_INPUT_CHEMICAL_2_IRI = EXP_3_BASE_IRI + 'InputChemical_2'
    EXP_3_INPUT_CHEMICAL_3_IRI = EXP_3_BASE_IRI + 'InputChemical_3'
    EXP_4_INPUT_CHEMICAL_1_IRI = EXP_4_BASE_IRI + 'InputChemical_1'
    EXP_4_INPUT_CHEMICAL_2_IRI = EXP_4_BASE_IRI + 'InputChemical_2'
    EXP_4_INPUT_CHEMICAL_3_IRI = EXP_4_BASE_IRI + 'InputChemical_3'
    EXP_5_INPUT_CHEMICAL_1_IRI = EXP_5_BASE_IRI + 'InputChemical_1'
    EXP_5_INPUT_CHEMICAL_2_IRI = EXP_5_BASE_IRI + 'InputChemical_2'
    EXP_5_INPUT_CHEMICAL_3_IRI = EXP_5_BASE_IRI + 'InputChemical_3'
    EXP_1_OUTPUT_CHEMICAL_IRI = EXP_1_BASE_IRI + 'OutputChemical_4'
    EXP_2_OUTPUT_CHEMICAL_IRI = EXP_2_BASE_IRI + 'OutputChemical_4'
    EXP_3_OUTPUT_CHEMICAL_IRI = EXP_3_BASE_IRI + 'OutputChemical_4'
    EXP_4_OUTPUT_CHEMICAL_IRI = EXP_4_BASE_IRI + 'OutputChemical_4'
    EXP_5_OUTPUT_CHEMICAL_IRI = EXP_5_BASE_IRI + 'OutputChemical_4'
    LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI = [EXP_1_INPUT_CHEMICAL_1_IRI, EXP_1_INPUT_CHEMICAL_2_IRI, EXP_1_INPUT_CHEMICAL_3_IRI]
    LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI = [EXP_2_INPUT_CHEMICAL_1_IRI, EXP_2_INPUT_CHEMICAL_2_IRI, EXP_2_INPUT_CHEMICAL_3_IRI]
    LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI = [EXP_3_INPUT_CHEMICAL_1_IRI, EXP_3_INPUT_CHEMICAL_2_IRI, EXP_3_INPUT_CHEMICAL_3_IRI]
    LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI = [EXP_4_INPUT_CHEMICAL_1_IRI, EXP_4_INPUT_CHEMICAL_2_IRI, EXP_4_INPUT_CHEMICAL_3_IRI]
    LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI = [EXP_5_INPUT_CHEMICAL_1_IRI, EXP_5_INPUT_CHEMICAL_2_IRI, EXP_5_INPUT_CHEMICAL_3_IRI]
    LIST_NEW_RXN_EXP_1_INPUT_CHEMICAL_IRI = [EXP_1_INPUT_CHEMICAL_1_IRI, EXP_1_INPUT_CHEMICAL_2_IRI, EXP_1_INPUT_CHEMICAL_3_IRI]
    LIST_NEW_RXN_EXP_2_INPUT_CHEMICAL_IRI = [EXP_1_INPUT_CHEMICAL_1_IRI, EXP_1_INPUT_CHEMICAL_2_IRI, EXP_1_INPUT_CHEMICAL_3_IRI]
    LIST_NEW_RXN_EXP_3_INPUT_CHEMICAL_IRI = [EXP_1_INPUT_CHEMICAL_1_IRI, EXP_1_INPUT_CHEMICAL_2_IRI, EXP_1_INPUT_CHEMICAL_3_IRI]
    LIST_RXN_EXP_1_OUTPUT_CHEMICAL_IRI = [EXP_1_OUTPUT_CHEMICAL_IRI]
    LIST_RXN_EXP_2_OUTPUT_CHEMICAL_IRI = [EXP_2_OUTPUT_CHEMICAL_IRI]
    LIST_RXN_EXP_3_OUTPUT_CHEMICAL_IRI = [EXP_3_OUTPUT_CHEMICAL_IRI]
    LIST_RXN_EXP_4_OUTPUT_CHEMICAL_IRI = [EXP_4_OUTPUT_CHEMICAL_IRI]
    LIST_RXN_EXP_5_OUTPUT_CHEMICAL_IRI = [EXP_5_OUTPUT_CHEMICAL_IRI]
    LIST_NEW_RXN_EXP_1_OUTPUT_CHEMICAL_IRI = None
    LIST_NEW_RXN_EXP_2_OUTPUT_CHEMICAL_IRI = None
    LIST_NEW_RXN_EXP_3_OUTPUT_CHEMICAL_IRI = None

    AUTOSAMPLER_SITE_CHEMICAL_MAPPING_DICT = {
        DUMMY_LAB_BASE_IRI + 'Site_3': EXP_1_INPUT_CHEMICAL_3_IRI,
        DUMMY_LAB_BASE_IRI + 'Site_1': EXP_1_INPUT_CHEMICAL_1_IRI,
        DUMMY_LAB_BASE_IRI + 'Site_2': EXP_1_INPUT_CHEMICAL_2_IRI
    }

    SPECIES_MOLAR_MASS_DICT = {"http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b": 106.12/1000,
        "http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3": 58.08/1000,
        'http://www.theworldavatar.com/kg/ontospecies/Species_f999de28-55dc-477e-8afc-e8802064e0d2': 146.19/1000,
        'http://www.theworldavatar.com/kg/ontospecies/Species_8765d201-0da9-4112-b653-3455002f535b': 234.29/1000,
        "http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202": 39.997/1000,
        "http://www.theworldavatar.com/kg/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb": 41.05/1000,
        "http://www.theworldavatar.com/kg/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983": 46.07/1000,
        "http://www.theworldavatar.com/kg/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340": 128.17/1000}

    RXNEXP_TYPE_DICT = {EXAMPLE_RXN_EXP_1_IRI:onto.ONTOREACTION_REACTIONEXPERIMENT,EXAMPLE_RXN_EXP_2_IRI:onto.ONTOREACTION_REACTIONEXPERIMENT,
        EXAMPLE_RXN_EXP_3_IRI:onto.ONTOREACTION_REACTIONEXPERIMENT,EXAMPLE_RXN_EXP_4_IRI:onto.ONTOREACTION_REACTIONEXPERIMENT,
        EXAMPLE_RXN_EXP_5_IRI:onto.ONTOREACTION_REACTIONEXPERIMENT,NEW_RXN_EXP_1_IRI:onto.ONTOREACTION_REACTIONVARIATION,
        NEW_RXN_EXP_2_IRI:onto.ONTOREACTION_REACTIONVARIATION,NEW_RXN_EXP_3_IRI:onto.ONTOREACTION_REACTIONVARIATION}
    RXNEXP_REACTION_CONDITION_DICT = {EXAMPLE_RXN_EXP_1_IRI:EXAMPLE_RXN_EXP_1_REACTION_CONDITION_IRI_LIST,EXAMPLE_RXN_EXP_2_IRI:EXAMPLE_RXN_EXP_2_REACTION_CONDITION_IRI_LIST,
        EXAMPLE_RXN_EXP_3_IRI:EXAMPLE_RXN_EXP_3_REACTION_CONDITION_IRI_LIST,EXAMPLE_RXN_EXP_4_IRI:EXAMPLE_RXN_EXP_4_REACTION_CONDITION_IRI_LIST,
        EXAMPLE_RXN_EXP_5_IRI:EXAMPLE_RXN_EXP_5_REACTION_CONDITION_IRI_LIST,NEW_RXN_EXP_1_IRI:NEW_RXN_EXP_1_REACTION_CONDITION_IRI_LIST,
        NEW_RXN_EXP_2_IRI:NEW_RXN_EXP_2_REACTION_CONDITION_IRI_LIST,NEW_RXN_EXP_3_IRI:NEW_RXN_EXP_3_REACTION_CONDITION_IRI_LIST}
    RXNEXP_PERFORMANCE_INDICATOR_DICT = {EXAMPLE_RXN_EXP_1_IRI:EXAMPLE_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST,EXAMPLE_RXN_EXP_2_IRI:EXAMPLE_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST,
        EXAMPLE_RXN_EXP_3_IRI:EXAMPLE_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST,EXAMPLE_RXN_EXP_4_IRI:EXAMPLE_RXN_EXP_4_PERFORMANCE_INDICATOR_IRI_LIST,
        EXAMPLE_RXN_EXP_5_IRI:EXAMPLE_RXN_EXP_5_PERFORMANCE_INDICATOR_IRI_LIST,NEW_RXN_EXP_1_IRI:NEW_RXN_EXP_1_PERFORMANCE_INDICATOR_IRI_LIST,
        NEW_RXN_EXP_2_IRI:NEW_RXN_EXP_2_PERFORMANCE_INDICATOR_IRI_LIST,NEW_RXN_EXP_3_IRI:NEW_RXN_EXP_3_PERFORMANCE_INDICATOR_IRI_LIST}
    RXNEXP_INPUT_CHEMICAL_DICT = {EXAMPLE_RXN_EXP_1_IRI:LIST_RXN_EXP_1_INPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_2_IRI:LIST_RXN_EXP_2_INPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_3_IRI:LIST_RXN_EXP_3_INPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_4_IRI:LIST_RXN_EXP_4_INPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_5_IRI:LIST_RXN_EXP_5_INPUT_CHEMICAL_IRI,NEW_RXN_EXP_1_IRI:LIST_NEW_RXN_EXP_1_INPUT_CHEMICAL_IRI,
        NEW_RXN_EXP_2_IRI:LIST_NEW_RXN_EXP_2_INPUT_CHEMICAL_IRI,NEW_RXN_EXP_3_IRI:LIST_NEW_RXN_EXP_3_INPUT_CHEMICAL_IRI}
    RXNEXP_OUTPUT_CHEMICAL_DICT = {EXAMPLE_RXN_EXP_1_IRI:LIST_RXN_EXP_1_OUTPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_2_IRI:LIST_RXN_EXP_2_OUTPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_3_IRI:LIST_RXN_EXP_3_OUTPUT_CHEMICAL_IRI,EXAMPLE_RXN_EXP_4_IRI:LIST_RXN_EXP_4_OUTPUT_CHEMICAL_IRI,
        EXAMPLE_RXN_EXP_5_IRI:LIST_RXN_EXP_5_OUTPUT_CHEMICAL_IRI,NEW_RXN_EXP_1_IRI:LIST_NEW_RXN_EXP_1_OUTPUT_CHEMICAL_IRI,
        NEW_RXN_EXP_2_IRI:LIST_NEW_RXN_EXP_2_OUTPUT_CHEMICAL_IRI,NEW_RXN_EXP_3_IRI:LIST_NEW_RXN_EXP_3_OUTPUT_CHEMICAL_IRI}
    RXNEXP_CHEMICAL_REACTION_IRI_DICT = {EXAMPLE_RXN_EXP_1_IRI:CHEMICAL_REACTION_IRI,EXAMPLE_RXN_EXP_2_IRI:CHEMICAL_REACTION_IRI,
        EXAMPLE_RXN_EXP_3_IRI:CHEMICAL_REACTION_IRI,EXAMPLE_RXN_EXP_4_IRI:CHEMICAL_REACTION_IRI,
        EXAMPLE_RXN_EXP_5_IRI:CHEMICAL_REACTION_IRI,NEW_RXN_EXP_1_IRI:CHEMICAL_REACTION_IRI,
        NEW_RXN_EXP_2_IRI:CHEMICAL_REACTION_IRI,NEW_RXN_EXP_3_IRI:CHEMICAL_REACTION_IRI}

    LIST_EXAMPLE_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI, EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_NEW_RXN_EXP = [NEW_RXN_EXP_1_IRI, NEW_RXN_EXP_2_IRI, NEW_RXN_EXP_3_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_DUMMY = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    # LIST_VAPR4_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_1_IRI, EXAMPLE_RXN_EXP_2_IRI]
    LIST_RXN_EXP_ASSIGNEDTO_VAPR4_ANOTHER_DUMMY = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    # LIST_VAPR4_ANOTHER_DUMMY_CONDUCTED_RXN_EXP = [EXAMPLE_RXN_EXP_3_IRI, EXAMPLE_RXN_EXP_4_IRI, EXAMPLE_RXN_EXP_5_IRI]
    LIST_DUMMY_R2PUMPS = [VAPOURTECR2PUMP_1_DUMMY_IRI, VAPOURTECR2PUMP_2_DUMMY_IRI, VAPOURTECR2PUMP_3_DUMMY_IRI, VAPOURTECR2PUMP_4_DUMMY_IRI]
    LIST_DUMMY_R4REACTORS = [VAPOURTECR4REACTOR_DUMMY_IRI, VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI]
    RXN_EXP_QUEUE_1 = 'https://example.com/blazegraph/namespace/test_temp/exp_1'
    RXN_EXP_QUEUE_2 = 'https://example.com/blazegraph/namespace/test_temp/exp_2'
    RXN_EXP_QUEUE_3 = 'https://example.com/blazegraph/namespace/test_temp/exp_3'
    RXN_EXP_QUEUE_4 = 'https://example.com/blazegraph/namespace/test_temp/exp_4'
    RXN_EXP_QUEUE_5 = 'https://example.com/blazegraph/namespace/test_temp/exp_5'
    RXN_EXP_QUEUE_6 = 'https://example.com/blazegraph/namespace/test_temp/exp_6'
    RXN_EXP_QUEUE_7 = 'https://example.com/blazegraph/namespace/test_temp/exp_7'
    RXN_EXP_1_PRIOR = []
    RXN_EXP_2_PRIOR = [RXN_EXP_QUEUE_1]
    RXN_EXP_3_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2]
    RXN_EXP_4_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3]
    RXN_EXP_5_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_6_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    RXN_EXP_7_PRIOR = [RXN_EXP_QUEUE_1, RXN_EXP_QUEUE_2, RXN_EXP_QUEUE_3, RXN_EXP_QUEUE_4]
    HPLCMETHOD_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCMethod_Dummy'
    PHASECOMPONENT_INTERNAL_STANDARD_IRI = 'https://www.example.com/triplestore/ontorxn/SinglePhase/PhaseComponent_InternalStandard'
    ONTOSPECIES_INTERNAL_STANDARD_IRI = 'http://www.theworldavatar.com/kg/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340'
    MOLARITY_INTERNAL_STANDARD = 0.05
    HPLCREPORT_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCReport_Dummy'
    HPLCJOB_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'HPLCJob_Dummy'
    HPLC_LOCAL_FOLDER_PATH = '/home/jb2197/CHEM32/**/'
    HPLCREPORT_DUMMY_REMOTE_PATH = 'http://placeholder_path'
    HPLCREPORT_DUMMY_LOCAL_PATH = 'placeholder_file_name'
    CHEMICAL_AMOUNT_FOR_DUMMY_OUTPUTCHEMICAL_IRI = DUMMY_LAB_BASE_IRI + 'ChemicalAmount_For_Dummy_OutputChemical'
    CHROMATOGRAMPOINT_1_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_1'
    CHROMATOGRAMPOINT_3_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_3'
    CHROMATOGRAMPOINT_4_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_4'
    CHROMATOGRAMPOINT_8_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_8'
    # CHROMATOGRAMPOINT_2_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_2'
    # CHROMATOGRAMPOINT_5_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_5'
    # CHROMATOGRAMPOINT_6_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_6'
    # CHROMATOGRAMPOINT_7_IRI = DUMMY_LAB_BASE_IRI + 'ChromatogramPoint_Dummy_7'
    LIST_CHROMATOGRAMPOINT_IRI = [
        CHROMATOGRAMPOINT_1_IRI,
        CHROMATOGRAMPOINT_3_IRI,
        CHROMATOGRAMPOINT_4_IRI,
        CHROMATOGRAMPOINT_8_IRI,
        # CHROMATOGRAMPOINT_2_IRI,
        # CHROMATOGRAMPOINT_5_IRI,
        # CHROMATOGRAMPOINT_6_IRI,
        # CHROMATOGRAMPOINT_7_IRI,
    ]
    DOE_BASE_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_1/'
    DOE_IRI = DOE_BASE_IRI + 'DoE_1'
    DOE_STRATEGY_IRI = DOE_BASE_IRI + 'Strategy_1'
    DOE_TSEMO_STRATEGY_IRI = DOE_STRATEGY_IRI
    DOE_DOMAIN_IRI = DOE_BASE_IRI + 'Domain_1'
    DOE_SYS_RES_1_IRI = DOE_BASE_IRI + 'SystemResponse_1'
    DOE_SYS_RES_2_IRI = DOE_BASE_IRI + 'SystemResponse_2'
    DOE_SYS_RES_IRI_LIST = [DOE_SYS_RES_1_IRI, DOE_SYS_RES_2_IRI]
    DOE_HIST_DATA_IRI = DOE_BASE_IRI + 'HistoricalData_1'
    DOE_HIST_DATE_REFERTO_IRI = LIST_EXAMPLE_RXN_EXP
    DOE_CONT_VAR_1_IRI = DOE_BASE_IRI + 'ContinuousVariable_1'
    DOE_CONT_VAR_2_IRI = DOE_BASE_IRI + 'ContinuousVariable_2'
    DOE_CONT_VAR_3_IRI = DOE_BASE_IRI + 'ContinuousVariable_3'
    DOE_CONT_VAR_4_IRI = DOE_BASE_IRI + 'ContinuousVariable_4'
    DOE_CONT_VAR_IRI_LIST = [DOE_CONT_VAR_1_IRI, DOE_CONT_VAR_2_IRI, DOE_CONT_VAR_3_IRI, DOE_CONT_VAR_4_IRI]
    DOE_SYS_RES_MAXIMISE_DICT = {DOE_SYS_RES_1_IRI:True, DOE_SYS_RES_2_IRI:False}

    DOE_NO_PRIOR_DATA_BASE_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_no_prior_data/'
    DOE_NO_PRIOR_DATA_IRI = DOE_NO_PRIOR_DATA_BASE_IRI + 'DoE_1'
    DOE_NO_PRIOR_DATA_STRATEGY_IRI = DOE_NO_PRIOR_DATA_BASE_IRI + 'Strategy_1'
    DOE_NO_PRIOR_DATA_DOMAIN_IRI = DOE_NO_PRIOR_DATA_BASE_IRI + 'Domain_1'
    DOE_NO_PRIOR_DATA_FIXED_PARAM_1_IRIR = DOE_NO_PRIOR_DATA_BASE_IRI + 'FixedParameter_1'
    DOE_NO_PRIOR_DATA_FIXED_PARAM_2_IRIR = DOE_NO_PRIOR_DATA_BASE_IRI + 'FixedParameter_2'
    DOE_NO_PRIOR_DATA_FIXED_PARAM_3_IRIR = DOE_NO_PRIOR_DATA_BASE_IRI + 'FixedParameter_3'
    DOE_NO_PRIOR_DATA_FIXED_PARAM_IRI_LIST = [DOE_NO_PRIOR_DATA_FIXED_PARAM_1_IRIR, DOE_NO_PRIOR_DATA_FIXED_PARAM_2_IRIR, DOE_NO_PRIOR_DATA_FIXED_PARAM_3_IRIR]

    DOE_TEMPLATE_BASE_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_Template/'
    DOE_TEMPLATE_IRI = DOE_TEMPLATE_BASE_IRI + 'DoE_1'
    DOE_TEMPLATE_DOMAIN_IRI = DOE_TEMPLATE_BASE_IRI + 'Domain_1'

    DUMMY_LAB_FOR_POST_PROC_BASE_IRI = 'https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/'
    HPLC_1_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'HPLC_1'
    HPLC_2_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'HPLC_2'
    CHEMICAL_AMOUNT_1_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'ChemicalAmount_1_1'
    CHEMICAL_AMOUNT_2_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'ChemicalAmount_2_1'
    VAPOURTECR4_1_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'VapourtecR4_1'
    VAPOURTECR4_2_POST_PROC_IRI = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'VapourtecR4_2'

    # NOTE some of the information below are commented out following the changes made to HPLCMETHOD_DUMMY_IRI in dummy_lab.ttl
    # this is to mimic the situation where either the species is not visible in the chromatogram or some information are not available/presented in the HPLC method
    HPLC_DUMMAY_REPORT_FILE_SPECIES_RETENTION_TIME_IDENTIFY = {
        'http://www.theworldavatar.com/kg/ontospecies/Species_54d8b46b-17bc-4bbd-a3cc-3b3a16d6ae4b': 0.55,
        # "http://www.theworldavatar.com/kg/ontospecies/Species_353d4667-e25d-476a-bd74-5c34723c8ea3": 2.55,
        'http://www.theworldavatar.com/kg/ontospecies/Species_f999de28-55dc-477e-8afc-e8802064e0d2': 1.55,
        'http://www.theworldavatar.com/kg/ontospecies/Species_8765d201-0da9-4112-b653-3455002f535b': 4.55,
        # "http://www.theworldavatar.com/kg/ontospecies/Species_eab77458-560d-4ce9-9b5e-96650fc3e202": 5.55,
        # "http://www.theworldavatar.com/kg/ontospecies/Species_0401f93b-b62d-488e-ba1f-7d5c37e365cb": 6.55,
        # "http://www.theworldavatar.com/kg/ontospecies/Species_63fefc5a-d49d-4841-a946-2cdb5f356983": 7.55,
        "http://www.theworldavatar.com/kg/ontospecies/Species_4fa4fdea-ed3d-4b0a-aee5-1f4e97dd2340": 3.55
    }

    RXNEXP_REACTOR_ASSIGNED_DICT = {EXAMPLE_RXN_EXP_1_IRI:VAPOURTECR4REACTOR_DUMMY_IRI,EXAMPLE_RXN_EXP_2_IRI:VAPOURTECR4REACTOR_DUMMY_IRI,
        EXAMPLE_RXN_EXP_3_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI,EXAMPLE_RXN_EXP_4_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI,
        EXAMPLE_RXN_EXP_5_IRI:VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI,NEW_RXN_EXP_1_IRI:VAPOURTECR4_1_POST_PROC_IRI,NEW_RXN_EXP_2_IRI:VAPOURTECR4_2_POST_PROC_IRI,NEW_RXN_EXP_3_IRI:None}

    INTENTIONALLY_OUT_OF_RANGE_BASE_IRI = 'https://intentionally_created_ReactionExperiment_out_of_range/'
    OUT_OF_RANGE_RXN_EXP_1_IRI = INTENTIONALLY_OUT_OF_RANGE_BASE_IRI + 'ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
    OUT_OF_RANGE_RXN_EXP_2_IRI = INTENTIONALLY_OUT_OF_RANGE_BASE_IRI + 'ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
    OUT_OF_RANGE_RXN_EXP_3_IRI = INTENTIONALLY_OUT_OF_RANGE_BASE_IRI + 'ReactionVariation_c4b175d9-e53c-4d7e-b053-3a81f7ca0ddf'
    YIELD_INTENTIONALLY_OUT_OF_RANGE_BASE_IRI = 'https://yield_out_of_range/'
    YIELD_OUT_OF_RANGE_RXN_EXP_1_IRI = YIELD_INTENTIONALLY_OUT_OF_RANGE_BASE_IRI + 'ReactionExperiment_negative_yield'
    YIELD_OUT_OF_RANGE_RXN_EXP_2_IRI = YIELD_INTENTIONALLY_OUT_OF_RANGE_BASE_IRI + 'ReactionExperiment_yield_too_high'
    LIST_INTENTIONALLY_OUT_OF_RANGE_RXN_EXP = [
        OUT_OF_RANGE_RXN_EXP_1_IRI, OUT_OF_RANGE_RXN_EXP_2_IRI, OUT_OF_RANGE_RXN_EXP_3_IRI,
        YIELD_OUT_OF_RANGE_RXN_EXP_1_IRI, YIELD_OUT_OF_RANGE_RXN_EXP_2_IRI
    ]
