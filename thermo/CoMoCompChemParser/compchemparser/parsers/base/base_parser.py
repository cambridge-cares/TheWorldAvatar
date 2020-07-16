from abc import ABC, abstractmethod

class BaseParser(ABC):

    def __init__(self):
        """ init the parser """

    @abstractmethod
    def parseLog(self,logFile,parsedResults):
        pass