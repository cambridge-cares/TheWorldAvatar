# In case you would like to use pydantic for data model, you can define your data classes here
import pydantic

import derivationagentpythonexample.data_model.iris as iris

class Value(pydantic.BaseModel):
    instance_iri: str
    clz: str = iris.DERIVATION_AGENT_PYTHON_EXAMPLE_VALUE
    numVal: float

class MaxValue(pydantic.BaseModel):
    instance_iri: str
    clz: str = iris.DERIVATION_AGENT_PYTHON_EXAMPLE_MAXVALUE
    hasValue: Value

class MinValue(pydantic.BaseModel):
    instance_iri: str
    clz: str = iris.DERIVATION_AGENT_PYTHON_EXAMPLE_MINVALUE
    hasValue: Value

class Difference(pydantic.BaseModel):
    instance_iri: str
    clz: str = iris.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE
    hasValue: Value
