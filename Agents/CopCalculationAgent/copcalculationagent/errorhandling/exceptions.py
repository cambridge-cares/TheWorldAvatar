class KGException(Exception):
    """Raise in case of exception when executing SPARQL queries/updates."""

class StackException(Exception):
    """Raise in case of exception when interacting with the Docker Stack."""

class InvalidInput(Exception):
    """Raise in case of an invalid input to the agent."""
