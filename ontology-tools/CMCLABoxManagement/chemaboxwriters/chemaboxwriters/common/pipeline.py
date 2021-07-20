class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(self, handler):
        self.current_handler= handler

    def add_handler(self, handler):
        return Pipeline(handler= lambda *args, **kwargs: handler(*self.current_handler(*args, **kwargs)))

    def execute(self, input, inputType):
        return self.current_handler(input, inputType)