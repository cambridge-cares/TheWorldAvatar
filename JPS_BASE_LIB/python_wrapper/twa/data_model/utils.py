from uuid import uuid4


def construct_namespace_iri(base_url: str, namespace: str):
    if base_url[-1] not in ['/', '#']:
        base_url += '/'
    return f'{base_url}{namespace}'

def construct_rdf_type(namespace_iri: str, class_name: str):
    if namespace_iri[-1] not in ['/', '#']:
        namespace_iri += '/'
    return f'{namespace_iri}{class_name}'

def init_instance_iri(namespace_iri: str, class_name: str):
    return f'{construct_rdf_type(namespace_iri, class_name)}_{str(uuid4())}'

def trim_iri(iri: str):
    return iri.rstrip('>').lstrip('<')
