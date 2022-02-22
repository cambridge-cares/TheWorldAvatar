class UnsupportedStage(Exception):
    """Raise for not supported abox processing stages."""

    pass


class UnsupportedPipeline(Exception):
    """Raise for not supported abox processing pipelines."""

    pass


class IncorrectHandlerParameter(Exception):
    """Raise for incorrect handler params."""

    pass


class IncorrectFileOrDirPath(Exception):
    """Raise for incorrect file or dir paths."""

    pass


class MissingUploadConfigs(Exception):
    """Raise for missing upload configs."""

    pass
