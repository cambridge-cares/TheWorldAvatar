from abc import ABC, abstractmethod

class base_postprocessor(ABC):

    def __init__(self):
        """ init the postprocessor """

    @abstractmethod
    def run(self):
        pass

