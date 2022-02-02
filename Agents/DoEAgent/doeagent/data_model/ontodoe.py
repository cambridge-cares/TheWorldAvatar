from pydantic.dataclasses import dataclass
from typing import List

from doeagent.data_model.ontorxn import *
from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

@dataclass
class Strategy:
    instance_iri: str

@dataclass
class TSEMO(Strategy):
    # this refers to the realisation of TSEMO algorithm in Summit python package
    # below are default value in Summit python package
    # more details, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo
    nRetries: int = 10
    nSpectralPoints: int = 1500
    nGenerations: int = 100
    populationSize: int = 100

@dataclass
class LHS(Strategy):
    seed: int
    # TODO add support for object property <hasCriterion> <OntoDoE:Criterion>

@dataclass
class DesignVariable:
    instance_iri: str
    name: str

@dataclass
class ContinuousVariable(DesignVariable):
    upperLimit: float
    lowerLimit: float
    positionalID: Optional[int]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

    def __post_init_post_parse__(self):
        # validate the upper and lower limit
        if self.upperLimit <= self.lowerLimit:
            raise Exception(
                'ContinuousVariable <%s> has an UpperLimit %s that is smaller then its LowerLimit %s.' 
                % (self.instance_iri, self.upperLimit, self.lowerLimit))

@dataclass
class CategoricalVariable(DesignVariable):
    pass

@dataclass
class Domain:
    instance_iri: str
    hasDesignVariable: List[DesignVariable]

@dataclass
class SystemResponse:
    instance_iri: str
    name: str
    maximise: bool
    positionalID: Optional[int]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

@dataclass
class HistoricalData:
    instance_iri: str
    refersTo: List[ReactionExperiment]
    numOfNewExp: int = 1

@dataclass
class DesignOfExperiment:
    instance_iri: Optional[str]
    usesStrategy: Strategy
    hasDomain: Domain
    hasSystemResponse: List[SystemResponse]
    utilisesHistoricalData: HistoricalData
    proposesNewExperiment: Optional[ReactionExperiment]
