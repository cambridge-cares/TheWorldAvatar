class BaseError(Exception):
    def __init__(self, message: str, code: int):
        super().__init__(message)
        self.message = message
        self.code = code

    def to_dict(self):
        return dict(
            error=dict(code=self.code, type=type(self).__name__, message=self.message)
        )


class MissingKgEndpointError(Exception):
    def __init__(self, kg: str):
        super().__init__("Missing endpoint for {kg}.".format(kg=kg))


class KgConnectionError(BaseError):
    def __init__(self):
        super().__init__("Unable to connect to knowledge base.", 500)
