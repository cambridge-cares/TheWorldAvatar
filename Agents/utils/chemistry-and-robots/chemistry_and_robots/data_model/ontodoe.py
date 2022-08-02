import pydantic
from typing import List, Optional

from chemistry_and_robots.data_model.iris import *
from pyderivationagent.data_model.utils import *

from chemistry_and_robots.data_model.base_ontology import BaseOntology
from chemistry_and_robots.data_model.ontoreaction import *

class Strategy(BaseOntology):
    pass

class TSEMO(Strategy):
    clz: str = ONTODOE_TSEMO
    # this refers to the realisation of TSEMO algorithm in Summit python package
    # below are default value in Summit python package
    # more details, please visit: https://gosummit.readthedocs.io/en/latest/strategies.html#tsemo
    nRetries: int = 10
    nSpectralPoints: int = 1500
    nGenerations: int = 100
    populationSize: int = 100

class LHS(Strategy):
    clz: str = ONTODOE_LHS
    seed: int
    # TODO add support for object property <hasCriterion> <OntoDoE:Criterion>

class DesignVariable(BaseOntology):
    clz: str = ONTODOE_DESIGNVARIABLE
    name: str

class ContinuousVariable(DesignVariable):
    clz: str = ONTODOE_CONTINUOUSVARIABLE
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
    clz: str = ONTODOE_CATEGORICALVARIABLE
    pass

class Domain(BaseOntology):
    clz: str = ONTODOE_DOMAIN
    hasDesignVariable: List[DesignVariable]

class SystemResponse(BaseOntology):
    clz: str = ONTODOE_SYSTEMRESPONSE
    name: str
    maximise: bool
    positionalID: Optional[int]
    # instead of the actual class, str is used to host the concept IRI of om:Quantity for simplicity
    refersTo: str

class HistoricalData(BaseOntology):
    clz: str = ONTODOE_HISTORICALDATA
    refersTo: List[ReactionExperiment]
    numOfNewExp: int = 1

class DesignOfExperiment(BaseOntology):
    clz: str = ONTODOE_DESIGNOFEXPERIMENT
    usesStrategy: Strategy
    hasDomain: Domain
    hasSystemResponse: List[SystemResponse]
    utilisesHistoricalData: HistoricalData
    proposesNewExperiment: Optional[ReactionExperiment]
