import pydantic
from typing import Any

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

class InstanceIRIInitialisationError(Exception):
    instance_iri_missing = """IRI of instance_iri is missing, please put it as pyasyncagent.data_model.utils.INSTANCE_IRI_TO_BE_INITIALISED \
    and provide namespace_for_init if you would like the instance_iri to be generated, \
    otherwise please put one that matches an existing instance IRI in the knowledge graph, \
    this error occurred when initialising a/an instance of: """
    clz_missing = 'IRI of clz is missing when initialising an instance of: '
    namespace_for_init_missing = 'IRI of namespace_for_init is missing when initialising an instance of: '
    both_clz_and_namespace_missing = 'Both IRI of clz and namespace_for_init are missing when initialising an instance of: '

    def __init__(self, value: str, message: str) -> None:
        full_message = message + value
        super().__init__(full_message)

class BaseOntology(pydantic.BaseModel):
    instance_iri: str = None
    clz: str = None
    namespace_for_init: str = None

    def __init__(__pydantic_self__, **data: Any) -> None:
        _instance_iri = data['instance_iri'] if 'instance_iri' in data else None
        _clz = data['clz'] if 'clz' in data else __pydantic_self__.__class__.__fields__['clz'].default
        _namespace_for_init = data['namespace_for_init'] if 'namespace_for_init' in data else None
        if _instance_iri is None:
            raise InstanceIRIInitialisationError(
                value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.instance_iri_missing
            ) if _clz is None else InstanceIRIInitialisationError(value=_clz, message=InstanceIRIInitialisationError.instance_iri_missing)
        elif data['instance_iri'] == INSTANCE_IRI_TO_BE_INITIALISED:
            if _clz is None and _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.both_clz_and_namespace_missing)
            elif _clz is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.clz_missing)
            elif _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=_clz, message=InstanceIRIInitialisationError.namespace_for_init_missing)
            else:
                data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], _clz)

        super().__init__(**data)
