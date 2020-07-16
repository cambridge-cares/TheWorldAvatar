from abc import ABC, abstractmethod

class BasePostprocessor(ABC):

    def __init__(self):
        """ init the postprocessor """

    @abstractmethod
    def run(self):
        pass

