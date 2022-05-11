from kgoperations.queryendpoints import SPARQL_ENDPOINTS
from kgoperations.querykg import querykg

def spec_inchi_query(inchi_string):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER((?Inchi) = "#INCHI").
    }
    """
    query = query.replace('#INCHI', inchi_string)
    return query