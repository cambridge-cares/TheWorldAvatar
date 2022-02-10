import pydantic
from typing import Any

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

class InstanceIRIInitialisationError(Exception):
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
        if data['instance_iri'] == INSTANCE_IRI_TO_BE_INITIALISED:
            _clz = data['clz'] if 'clz' in data else __pydantic_self__.__class__.__fields__['clz'].default
            _namespace_for_init = data['namespace_for_init'] if 'namespace_for_init' in data else None
            if _clz is None and _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.both_clz_and_namespace_missing)
            elif _clz is None:
                raise InstanceIRIInitialisationError(value=str(__pydantic_self__.__class__), message=InstanceIRIInitialisationError.clz_missing)
            elif _namespace_for_init is None:
                raise InstanceIRIInitialisationError(value=_clz, message=InstanceIRIInitialisationError.namespace_for_init_missing)
            else:
                data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], _clz)

        super().__init__(**data)
