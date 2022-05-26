import uuid

INSTANCE_IRI_TO_BE_INITIALISED = 'INSTANCE_IRI_TO_BE_INITIALISED'

def initialiseInstanceIRI(namespace: str, clz: str) -> str:
    return checkNamespace(namespace) + getShortName(clz) + '_' + str(uuid.uuid4())

def getShortName(iri):
    """
        This function gets the final part after the last '#' or '/' of an IRI.
        For example, it will return 'RxnExp_1' given 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1#RxnExp_1' or 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/RxnExp_1'.

        Arguments:
            iri - IRI of interest
    """
    iri = trimIRI(iri)
    # Raise exception if the provided IRI ends with '#' or '/'
    if iri.endswith('#') or iri.endswith('/'):
        raise Exception(f"The IRI <{iri}> is not provided in correct format. It should NOT end with '#' or '/' when retrieving its shortname.")
    if '#' in iri:
        temp_iri = iri[iri.rfind('#')+1:]
        # Check if the parts after '#' is one string without separation, i.e. it should be 'shortname', instead of 'short/name'
        if not '/' in temp_iri:
            return temp_iri
        else:
            # Make sure return only the final part of the shortname
            return temp_iri[temp_iri.rfind('/')+1:]
    else:
        return iri[iri.rfind('/')+1:]

def getNameSpace(iri):
    """
        This method gets the namespace of a given IRI.
        For example, it will return 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1#' given 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1#RxnExp_1'.
        If given 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/RxnExp_1', it will return 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/'.

        Arguments:
            iri - IRI of interest
    """
    iri = trimIRI(iri)
    if '#' in iri:
        return iri[:iri.rfind('#')+1]
    else:
        return iri[:iri.rfind('/')+1]

def checkNamespace(namespace):
    if namespace.startswith('http://') or namespace.startswith('https://'):
        return namespace if namespace.endswith('/') else namespace + '/'
    else:
        raise Exception("The provide namespace should start with either 'http://' or 'https://'.")

def trimIRI(iri):
    """
        This method deletes the '<' and '>' around the given IRI (or lists of IRIs).
        Arguments:
            iri - IRI of interest
    """
    if isinstance(iri, list):
        for i in range(len(iri)):
            iri[i] = trimIRI(iri[i])
    else:
        if iri.startswith("<"):
            iri = iri[1:]
        if iri.endswith(">"):
            iri = iri[:-1]
    return iri
