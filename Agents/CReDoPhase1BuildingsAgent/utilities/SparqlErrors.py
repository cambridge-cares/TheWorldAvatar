# Define custom exceptions

class Error(Exception):
    """Base class for other exceptions"""
    pass


class QueryError(Error):
    """Raised for SPARQL query induced errors """
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)


class ResultsError(Error):
    """Raised for erroneous SPARQL results"""
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)
