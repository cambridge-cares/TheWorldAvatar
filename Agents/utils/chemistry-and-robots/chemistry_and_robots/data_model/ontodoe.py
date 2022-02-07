import pydantic
from typing import List, Optional

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontorxn import *

class Strategy(BaseOntology):
    pass

class TSEMO(Strategy):
    # this refers to the realisation of TSEMO algorithm in Summit python package
    # below are default value in Summit python package
    # more details, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo
    nRetries: int = 10
    nSpectralPoints: int = 1500
    nGenerations: int = 100
    populationSize: int = 100

class LHS(Strategy):
    seed: int
    # TODO add support for object property <hasCriterion> <OntoDoE:Criterion>

class DesignVariable(BaseOntology):
    name: str

class ContinuousVariable(DesignVariable):
    upperLimit: float
    lowerLimit: float
    positionalID: Optional[int]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

    @pydantic.root_validator
    @classmethod
    def upper_and_lower_limit(cls, values):
        # validate the upper and lower limit
        if values.get('upperLimit') <= values.get('lowerLimit'):
            raise Exception(
                'ContinuousVariable <%s> has an UpperLimit %s that is smaller then its LowerLimit %s.' 
                % (values.get('instance_iri'), values.get('upperLimit'), values.get('lowerLimit')))
        return values

class CategoricalVariable(DesignVariable):
    pass

class Domain(BaseOntology):
    hasDesignVariable: List[DesignVariable]

class SystemResponse(BaseOntology):
    name: str
    maximise: bool
    positionalID: Optional[int]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

class HistoricalData(BaseOntology):
    refersTo: List[ReactionExperiment]
    numOfNewExp: int = 1

class DesignOfExperiment(BaseOntology):
    usesStrategy: Strategy
    hasDomain: Domain
    hasSystemResponse: List[SystemResponse]
    utilisesHistoricalData: HistoricalData
    proposesNewExperiment: Optional[ReactionExperiment]
