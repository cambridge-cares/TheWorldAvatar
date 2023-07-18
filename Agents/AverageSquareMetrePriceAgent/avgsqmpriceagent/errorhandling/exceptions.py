class KGException(Exception):
    """Raise in case of exception when executing SPARQL queries/updates."""

class TSException(Exception):
    """Raise in case of exception when using the TimeSeriesClient."""

class StackException(Exception):
    """Raise in case of exception when interacting with the Docker Stack."""

class APIException(Exception):
    """Raise in case of exception when interacting with EPC API."""
