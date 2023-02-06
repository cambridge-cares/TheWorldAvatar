"""
# Author: qhouyee #

A module that provides all pytest fixtures and utility functions for all integration tests.
"""

# Third party import
import pytest

# Self import
from agent.kgutils import KGClient
from . import testconsts


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="session")
def initialise_client():
    """
    Retrieves all the exposed endpoints for dockerised testing services
    """
    # Create KG Client for testing
    kg_client = KGClient(testconsts.KG_ENDPOINT, testconsts.KG_ENDPOINT)

    # Returns client for the test to continue running
    yield kg_client

    # Clean up operations at the end of the test
    clear_triplestore(kg_client)
    clear_loggers()


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def clear_loggers():
    """Remove handlers from all loggers. Adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)

def clear_triplestore(kgClient):
    """Delete all triples"""
    query_delete = """
        DELETE WHERE {?s ?p ?o}
        """
    kgClient.execute_update(query_delete)
