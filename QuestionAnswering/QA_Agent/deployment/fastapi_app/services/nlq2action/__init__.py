from .translate import Nlq2ActionTranslator


class Nlq2ActionExecutor:
    def __init__(self, translator: Nlq2ActionTranslator):
        self.translator = translator