from testcontainers.core.container import DockerContainer
from pathlib import Path
import logging
import pytest
import time

logging.getLogger("py4j").setLevel(logging.INFO)

from hplcinputagent.agent import *

pytest_plugins = ["docker_compose"]

HPLC_DIGITAL_TWIN = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/HPLC_Dummy'
HPLC_REPORT_PERIODIC_TIMESCALE = 6
HPLC_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__HPLCInput#Service'
DERIVATION_PERIODIC_TIMESCALE = 120
DERIVATION_INSTANCE_BASE_URL = 'http://localhost:8080/ontolab/'

def test_hplc_input_agent(initialise_triples):
    sparql_client, sparql_endpoint, file_service_url = initialise_triples

    hplc_input_agent = HPLCInputAgent(
        hplc_digital_twin=HPLC_DIGITAL_TWIN, hplc_report_periodic_timescale=HPLC_REPORT_PERIODIC_TIMESCALE,
        file_server_upload=file_service_url,
        agent_iri=HPLC_ONTOAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL, kg_url=sparql_endpoint, logger_name='dev'
    )

    hplc_input_agent.start_monitoring_local_report_folder()
