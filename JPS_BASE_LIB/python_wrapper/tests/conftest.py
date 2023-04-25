from testcontainers.core.container import DockerContainer

import logging
import pytest
import time

logging.getLogger("py4j").setLevel(logging.INFO)


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def initialise_triple_store():
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer(
        'ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0')
    # the port is set as 9999 to match with the value set in the docker image
    blazegraph.with_exposed_ports(9999)

    with blazegraph as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        yield endpoint

        # Clear logger at the end of the test
        clear_loggers()


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
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
