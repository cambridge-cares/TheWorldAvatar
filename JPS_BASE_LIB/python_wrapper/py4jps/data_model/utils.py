from uuid import uuid4

# from py4jps.data_model.iris import TWA_BASE_PREFIX

def construct_rdf_type(base_prefix: str, namespace: str, class_name: str):
    if base_prefix[-1] not in ['/', '#']:
        base_prefix += '/'
    return f'{base_prefix}{namespace}/{class_name}'

def init_instance_iri(base_prefix: str, class_name: str):
    if base_prefix[-1] not in ['/', '#']:
        base_prefix += '/'
    return f'{base_prefix}{class_name}_{str(uuid4())}'

def trim_iri(iri: str):
    return iri.rstrip('>').lstrip('<')
