from rdflib import Graph
from pathlib import Path

import chemistry_and_robots.tests.conftest as conftest

import logging
logger = logging.getLogger(__file__)

def test_parse_ttl():
    """Test that all the example triples can be parsed, hence can be uploaded to the triple store.

    Raises:
        e: Any exception that is raised by the rdflib parser.
    """
    g = Graph()
    pathlist = Path(conftest.RESOURCE_DIR).glob('**/*.ttl')
    for path in pathlist:
        try:
            g.parse(str(path))
        except Exception as e:
            logger.error(f"Failed to parse {path} with error {e}", exc_info=True)
            raise e
