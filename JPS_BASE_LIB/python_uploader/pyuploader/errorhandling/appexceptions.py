class EnvironmentVarError(Exception):
    pass

class NotSupportedUploader(Exception):
    pass

class UploaderInputError(Exception):
    pass

class FileUploadError(Exception):
    pass


class SpecsFileNotFoundError(Exception):
    pass

class SecretsFileNotFoundError(Exception):
    pass

class WrongSecretsFormatError(Exception):
    pass