################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

class InvalidInput(Exception):
    """Raise in case of an invalid input to the agent."""

class ConnectionException(Exception):
    """Raise in case of an connection to agent fails."""

class StackException(Exception):
    """Raise in case of exception when interacting with the Docker Stack."""

class KGException(Exception):
    """Raise in case of exception when executing SPARQL queries/updates."""

