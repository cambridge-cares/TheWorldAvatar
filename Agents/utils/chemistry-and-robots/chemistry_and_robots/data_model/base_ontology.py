import pydantic
from typing import Any

from pyasyncagent.data_model.iris import *
from pyasyncagent.data_model.utils import *

class InstanceIRIInitialisationError(Exception):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)

class BaseOntology(pydantic.BaseModel):
    instance_iri: str = None
    clz: str = None
    namespace_for_init: str = None

    def __init__(__pydantic_self__, **data: Any) -> None:
        if data['instance_iri'] == INSTANCE_IRI_TO_BE_INITIALISED:
            if data['namespace_for_init'] is None:
                raise InstanceIRIInitialisationError(f"A namespace should be provided for initialising a/an {data['clz'] if 'clz' in data else __pydantic_self__.__class__.__fields__['clz'].default} instance.")
            else:
                if 'clz' not in data:
                    data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], __pydantic_self__.__class__.__fields__['clz'].default)
                else:
                    data['instance_iri'] = initialiseInstanceIRI(data['namespace_for_init'], data['clz'])
        super().__init__(**data)
