class UnsupportedStage(Exception):
    """Raise for not supported abox processing stages."""

    pass


class UnsupportedPipeline(Exception):
    """Raise for not supported abox processing pipelines."""

    pass


class IncorrectHandlerParameter(Exception):
    """Raise for incorrect handler params."""

    pass


class IncorrectInputArgument(Exception):
    """Raise for incorrect input argument."""

    pass


class IncorrectFileOrDirPath(Exception):
    """Raise for incorrect file or dir paths."""

    pass


class MissingUploadConfigs(Exception):
    """Raise for missing upload configs."""

    pass


class MissingRequiredInput(Exception):
    """Raise for missing input that is required."""

    pass


class MissingHandlerConfig(Exception):
    """Raise for missing handler config."""

    pass


class MissingInstance(Exception):
    """Missing instance."""

    pass


class MissingOntologyName(Exception):
    """Missing ontology name."""

    pass


class StoreClientUpdateEndpointNotSetError(Exception):
    "Update endpoint not set."

    pass


class MissingQueryEndpoint(Exception):
    "Missing query endpoint."

    pass


class WrongHandlerArgType(Exception):
    "Wrong handler argument type."

    pass


class MissingUploadEndpoint(Exception):
    "Missing upload endpoint."

    pass


class MissingUploadEndpointAuthorisation(Exception):
    "Missing upload endpoint authorisation."

    pass


class MismatchedSchemaListVariablesLength(Exception):
    "Schema list variables have different lengths."

    pass
