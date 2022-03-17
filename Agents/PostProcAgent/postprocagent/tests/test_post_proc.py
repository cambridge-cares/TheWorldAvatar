from testcontainers.core.container import DockerContainer
from pathlib import Path
import logging
import pytest
import time

logging.getLogger("py4j").setLevel(logging.INFO)

from postprocagent.agent import *

pytest_plugins = ["docker_compose"]

HPLC_DIGITAL_TWIN = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/HPLC_Dummy'
HPLC_REPORT_PERIODIC_TIMESCALE = 6
POSTPROC_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__PostProc#Service'
DERIVATION_PERIODIC_TIMESCALE = 120
DERIVATION_INSTANCE_BASE_URL = 'http://localhost:8080/ontolab/'

def test_post_proc_agent(initialise_triples):
    sparql_client, sparql_endpoint, file_service_url, fs_user, fs_pwd = initialise_triples

    post_proc_agent = PostProcAgent(
        fs_url=file_service_url, fs_user=fs_user, fs_pwd=fs_pwd,
        agent_iri=POSTPROC_ONTOAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL, kg_url=sparql_endpoint, logger_name='dev'
    )

    post_proc_agent.start_monitoring_derivations()

    time.sleep(6000)
