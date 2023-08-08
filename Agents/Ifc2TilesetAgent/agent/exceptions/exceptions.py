"""
# Author: qhouyee #

This module provides the exception types raised in the agent.
"""


class InvalidInputError(Exception):
    """Raise in case of an invalid input to the agent."""


class KGException(Exception):
    """Raise in case of exception when executing SPARQL queries/updates."""


class QueryBuilderError(Exception):
    """Raise in case of an invalid query syntax passed to the query builder."""
