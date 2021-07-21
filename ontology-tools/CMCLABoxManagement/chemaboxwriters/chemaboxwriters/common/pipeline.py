class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """
    def __init__(self):
        self.handlers= []

    def add_handler(self, handler):
        self.handlers.append(handler)
        return self
        #Pipeline(handler= lambda *args, **kwargs: handler(*self.current_handler(*args, **kwargs)))

    #def execute(self, input, inputType):
    #    return self.current_handler(input, inputType)

    def execute(self, inputs, inputType, outDir=None, outBaseName=None):
        for handler in self.handlers:
            inputs, inputType = handler.run(inputs, inputType, outDir, outBaseName)
        return inputs, inputType