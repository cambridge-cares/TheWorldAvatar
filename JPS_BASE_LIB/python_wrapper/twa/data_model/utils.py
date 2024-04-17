from uuid import uuid4


def construct_namespace_iri(base_url: str, namespace: str) -> str:
    """
    This function constructs the namespace IRI from the base URL and namespace.
    For example, if the base URL is "https://www.theworldavatar.com/kg" and the namespace is "ontolab",
    The function will return "https://www.theworldavatar.com/kg/ontolab".

    Args:
        base_url (str): The base URL of the namespace IRI, e.g. "https://www.theworldavatar.com/kg"
        namespace (str): The namespace, e.g. "ontolab", will be ignored if None

    Returns:
        str: The namespace IRI, e.g. "https://www.theworldavatar.com/kg/ontolab"
    """
    if base_url[-1] not in ['/', '#']:
        base_url += '/'
    return f'{base_url}{namespace}' if namespace is not None else base_url


def construct_rdf_type(namespace_iri: str, class_name: str) -> str:
    """
    This function constructs the RDF type IRI from the namespace IRI and class name.

    Args:
        namespace_iri (str): The namespace IRI, e.g. "https://www.theworldavatar.com/kg/ontolab"
        class_name (str): The class name, e.g. "LabEquipment"

    Returns:
        str: The RDF type IRI, e.g. "https://www.theworldavatar.com/kg/ontolab/LabEquipment"
    """
    if namespace_iri[-1] not in ['/', '#']:
        namespace_iri += '/'
    return f'{namespace_iri}{class_name}'


def init_instance_iri(namespace_iri: str, class_name: str) -> str:
    """
    The function constructs a unique IRI for an instance of a class in a namespace.

    Args:
        namespace_iri (str): The namespace IRI, e.g. "https://www.theworldavatar.com/kg/ontolab"
        class_name (str): The class name, e.g. "LabEquipment"

    Returns:
        str: The unique IRI for the instance, e.g. "https://www.theworldavatar.com/kg/ontolab/LabEquipment_12345678"
    """
    return f'{construct_rdf_type(namespace_iri, class_name)}_{str(uuid4())}'


def trim_iri(iri: str) -> str:
    """
    This function trims the "<" and ">" characters from the left and right side of an IRI.

    Args:
        iri (str): The IRI to be trimmed

    Returns:
        str: The trimmed IRI
    """
    return iri.rstrip('>').lstrip('<')
