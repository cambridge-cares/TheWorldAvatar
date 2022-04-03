class InvalidInput(Exception):
    """Raise in case of an invalid input to the agent."""

class KGException(Exception):
    """Raise in case of exception when executing SPARQL queries/updates."""

class APIException(Exception):
    """Raise in case of exception when interacting with Met Office DataPoint API."""