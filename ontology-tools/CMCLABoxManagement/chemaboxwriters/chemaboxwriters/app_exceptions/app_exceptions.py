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
