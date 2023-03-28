################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 28 Feb 2023                            #
################################################

# The purpose of this module is to provide templates for
# all required types SPARQL queries

def get_property_query(ontological_class: str) -> str:
    """
    Generates a SPARQL query to retrieve all datatype properties and
    object properties directly connected to the ontological
    class through the domain relationship.

    Parameters:
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: A SPARQL query to retrieve all directly connected properties of
    the ontological class.
    """
    query = '''
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontomatpassport: <http://www.theworldavatar.com/kg/ontomatpassport#>

    SELECT DISTINCT ?property ?label ?position ?type ?range ?rangeLabel
    WHERE {
        {
        ?property rdf:type owl:ObjectProperty .
        ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain ?domain .
        ?domain owl:unionOf/rdf:rest*/rdf:first  %s .
        ?property rdfs:range ?intermediateRange .
        ?intermediateRange owl:unionOf/rdf:rest*/rdf:first ?range .
        ?range rdfs:label ?rangeLabel .
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION
    {
        ?property rdf:type owl:ObjectProperty .
        ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain ?domain .
        ?domain owl:unionOf/rdf:rest*/rdf:first  %s .
        ?property rdfs:range ?range .
        FILTER (contains(str(?range), 'http:') || contains(str(?range), 'https:'))
        ?range rdfs:label ?rangeLabel .
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION
        {
        ?property rdf:type owl:ObjectProperty .
                ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain  %s .
        ?property rdfs:range ?range .
        FILTER (contains(str(?range), 'http:') || contains(str(?range), 'https:'))
        ?range rdfs:label ?rangeLabel .
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION
        {
        ?property rdf:type owl:DatatypeProperty .
                ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain ?domain .
        ?domain owl:unionOf/rdf:rest*/rdf:first  %s .
        ?property rdfs:range ?rangeInitial .
        BIND(REPLACE(STR(?rangeInitial), "xsd:", "") AS ?rangeXSDRemoved)
        BIND(REPLACE(STR(?rangeXSDRemoved), "owl:", "") AS ?rangeOWLRemoved)
        BIND(REPLACE(STR(?rangeOWLRemoved), "rdfs:", "") AS ?rangeRDFSRemoved)
        BIND(REPLACE(STR(?rangeRDFSRemoved), "rdf:", "") AS ?range)
        FILTER (?range != "")
        ?property rdfs:range ?rangeLabelInitial .
        BIND(REPLACE(STR(?rangeLabelInitial), "http://www.w3.org/2001/XMLSchema#", "") AS ?rangeLabelXSDRemoved)
        BIND(REPLACE(STR(?rangeLabelXSDRemoved), "http://www.w3.org/2002/07/owl#", "") AS ?rangeLabelOWLRemoved)
        BIND(REPLACE(STR(?rangeLabelOWLRemoved), "http://www.w3.org/2000/01/rdf-schema#", "") AS ?rangeLabelRDFSRemoved)
        BIND(REPLACE(STR(?rangeLabelRDFSRemoved), "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "") AS ?rangeLabel)
        FILTER (?rangeLabel != "")
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION{
        ?property rdf:type owl:DatatypeProperty .
        ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain  %s .
        ?property rdfs:range ?rangeInitial .
        BIND(REPLACE(STR(?rangeInitial), "xsd:", "") AS ?rangeXSDRemoved)
        BIND(REPLACE(STR(?rangeXSDRemoved), "owl:", "") AS ?rangeOWLRemoved)
        BIND(REPLACE(STR(?rangeOWLRemoved), "rdfs:", "") AS ?rangeRDFSRemoved)
        BIND(REPLACE(STR(?rangeRDFSRemoved), "rdf:", "") AS ?range)
        FILTER (?range != "")
        ?property rdfs:range ?rangeLabelInitial .
        BIND(REPLACE(STR(?rangeLabelInitial), "http://www.w3.org/2001/XMLSchema#", "") AS ?rangeLabelXSDRemoved)
        BIND(REPLACE(STR(?rangeLabelXSDRemoved), "http://www.w3.org/2002/07/owl#", "") AS ?rangeLabelOWLRemoved)
        BIND(REPLACE(STR(?rangeLabelOWLRemoved), "http://www.w3.org/2000/01/rdf-schema#", "") AS ?rangeLabelRDFSRemoved)
        BIND(REPLACE(STR(?rangeLabelRDFSRemoved), "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "") AS ?rangeLabel)
        FILTER (?rangeLabel != "")
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION
        {
        ?property rdf:type owl:DatatypeProperty .
                ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain ?domain .
        ?domain owl:unionOf/rdf:rest*/rdf:first  %s .
        ?property rdfs:range ?rangeInitial .
        BIND(REPLACE(STR(?rangeInitial), "xsd:", "") AS ?rangeXSDRemoved)
        BIND(REPLACE(STR(?rangeXSDRemoved), "owl:", "") AS ?rangeOWLRemoved)
        BIND(REPLACE(STR(?rangeOWLRemoved), "rdfs:", "") AS ?rangeRDFSRemoved)
        BIND(REPLACE(STR(?rangeRDFSRemoved), "rdf:", "") AS ?range)
        FILTER (?range != "")
        ?property rdfs:range ?rangeValue .
      	?rangeValue owl:oneOf ?list .
  		?list rdf:rest*/rdf:first ?rangeLabelInitial .
        BIND(REPLACE(STR(?rangeLabelInitial), "http://www.w3.org/2001/XMLSchema#", "") AS ?rangeLabelXSDRemoved)
        BIND(REPLACE(STR(?rangeLabelXSDRemoved), "http://www.w3.org/2002/07/owl#", "") AS ?rangeLabelOWLRemoved)
        BIND(REPLACE(STR(?rangeLabelOWLRemoved), "http://www.w3.org/2000/01/rdf-schema#", "") AS ?rangeLabelRDFSRemoved)
        BIND(REPLACE(STR(?rangeLabelRDFSRemoved), "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "") AS ?rangeLabel)
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    UNION{
        ?property rdf:type owl:DatatypeProperty .
        ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:domain %s .
        ?property rdfs:range ?rangeInitial .
        BIND(REPLACE(STR(?rangeInitial), "xsd:", "") AS ?rangeXSDRemoved)
        BIND(REPLACE(STR(?rangeXSDRemoved), "owl:", "") AS ?rangeOWLRemoved)
        BIND(REPLACE(STR(?rangeOWLRemoved), "rdfs:", "") AS ?rangeRDFSRemoved)
        BIND(REPLACE(STR(?rangeRDFSRemoved), "rdf:", "") AS ?range)
        FILTER (?range != "")
        ?property rdfs:range ?rangeValue .
      	?rangeValue owl:oneOf ?list .
  		?list rdf:rest*/rdf:first ?rangeLabelInitial .
        BIND(REPLACE(STR(?rangeLabelInitial), "http://www.w3.org/2001/XMLSchema#", "") AS ?rangeLabelXSDRemoved)
        BIND(REPLACE(STR(?rangeLabelXSDRemoved), "http://www.w3.org/2002/07/owl#", "") AS ?rangeLabelOWLRemoved)
        BIND(REPLACE(STR(?rangeLabelOWLRemoved), "http://www.w3.org/2000/01/rdf-schema#", "") AS ?rangeLabelRDFSRemoved)
        BIND(REPLACE(STR(?rangeLabelRDFSRemoved), "http://www.w3.org/1999/02/22-rdf-syntax-ns#", "") AS ?rangeLabel)
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    }
    ORDER BY ?position
    ''' % (ontological_class, ontological_class, ontological_class, ontological_class, ontological_class, ontological_class, ontological_class)
    return query

def get_subclass_query(ontological_class: str) -> str:
    """
    Generates a SPARQL query to retrieve all subclasses directly
    connected to the ontological class through the inverse
    subclass of relationship.

    Parameters:
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: A SPARQL query to retrieve all directly connected subclasses of
    the ontological class.
    """
    query = '''
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontomatpassport: <http://www.theworldavatar.com/kg/ontomatpassport#>

    SELECT ?property ?label ?position ?type
    WHERE {
        %s ^rdfs:subClassOf ?property .
        ?property rdf:type ?type .
        FILTER (?type != owl:NamedIndividual)
        ?property rdfs:label ?label .
        ?property ontomatpassport:hasGUIPosition ?position .
    }
    ORDER BY ?position
    ''' % ontological_class
    return query

def get_instance_query(ontological_class: str) -> str:
    """
    Generates a SPARQL query to retrieve all instances of
    the ontological class through the RDF type relationship.

    Parameters:
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: A SPARQL query to retrieve all instances of the ontological class.
    """
    query = '''
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?property ?label
    WHERE {
        ?property rdf:type %s .
        ?property rdfs:label ?label .
    } ORDER BY ?label
    ''' % ontological_class
    return query

def get_unit_query(ontological_class: str) -> str:
    """
    Generates a SPARQL query to retrieve the unit linked to
    the ontological class through the has unit relationship.

    Parameters:
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: A SPARQL query to retrieve the unit linked to the ontological class.
    """
    query = '''
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontomatpassport: <http://www.theworldavatar.com/kg/ontomatpassport#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

    SELECT ?unit ?symbol
    WHERE {
        %s rdfs:subClassOf*/owl:hasValue ?unit .
        ?unit om:symbol ?symbol
    }
    ''' % ontological_class
    return query

def get_label_query(ontological_element: str) -> str:
    """
    Generates a SPARQL query to retrieve the label of an
    ontological class or property.

    Parameters:
    ontological_element (str): The IRI of the ontological class or property.

    Returns:
    str: A SPARQL query to retrieve the label linked to the ontological class
    of property via the rdfs:label property.
    """
    query = '''
    PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>

    SELECT ?label
    WHERE {
        %s rdfs:label ?label .
    }
    ''' % ontological_element
    return query
