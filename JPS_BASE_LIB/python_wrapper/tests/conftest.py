from testcontainers.core.container import DockerContainer

import requests
import logging
import pytest
import time

from py4jps.kg_operations import PySparqlClient

logging.getLogger("py4j").setLevel(logging.INFO)


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def initialise_triple_store():
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer(
        'ghcr.io/cambridge-cares/blazegraph:1.1.0')
    # the port is set as 8080 to match with the value set in the docker image
    blazegraph.with_exposed_ports(8080)

    with blazegraph as container:
        # It ensures that the requested Blazegraph Docker service is ready to accept SPARQL query/update
        service_available = False
        while not service_available:
            try:
                # Retrieve SPARQL endpoint
                endpoint = get_endpoint(container)
                response = requests.head(endpoint)
                if response.status_code != requests.status_codes.codes.not_found:
                    service_available = True
            except (requests.exceptions.ConnectionError, TypeError):
                time.sleep(3)

        yield endpoint

        # Clear logger at the end of the test
        clear_loggers()


@pytest.fixture(scope="module")
def initialise_sparql_client(initialise_triple_store):
    sparql_endpoint = initialise_triple_store
    sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
    sparql_client.perform_update('delete where { ?s ?p ?o }')
    yield sparql_client
    sparql_client.perform_update('delete where { ?s ?p ?o }')


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(8080)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
