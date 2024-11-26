from uuid import uuid4
from typing import Union, List


def check_valid_url(url: str) -> str:
    """
    This function checks if the provided URL for namespace starts with "http://" or "https://".
    If so, it returns the URL and add "/" if it's not already ending with a "/" or "#".

    Args:
        url (str): The URL to be checked

    Raises:
        Exception: The URL is not provided with either "http://" or "https://" as its start

    Returns:
        str: The original URL or the processed URL with a "/" added at its end
    """
    if url.startswith('http://') or url.startswith('https://'):
        return url if url[-1] in ['/', '#'] else url + '/'
    else:
        raise Exception("The provide url for namespace should start with either 'http://' or 'https://'.")


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
    return f'{check_valid_url(base_url)}{namespace}' if namespace is not None else base_url


def construct_rdf_type(namespace_iri: str, class_name: str) -> str:
    """
    This function constructs the RDF type IRI from the namespace IRI and class name.

    Args:
        namespace_iri (str): The namespace IRI, e.g. "https://www.theworldavatar.com/kg/ontolab"
        class_name (str): The class name, e.g. "LabEquipment"

    Returns:
        str: The RDF type IRI, e.g. "https://www.theworldavatar.com/kg/ontolab/LabEquipment"
    """
    return f'{check_valid_url(namespace_iri)}{class_name}'


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


def trim_iri(iri: Union[str, List[str]]) -> Union[str, List[str]]:
    """
    This function trims the "<" and ">" characters from the left and right side of the given IRI (or lists of IRIs).

    Args:
        iri (str or list): The IRI(s) to be trimmed

    Returns:
        str: The trimmed IRI
    """
    if isinstance(iri, list):
        for i in range(len(iri)):
            iri[i] = trim_iri(iri[i])
    else:
        iri = iri.strip().lstrip("<").rstrip(">")
    return iri
