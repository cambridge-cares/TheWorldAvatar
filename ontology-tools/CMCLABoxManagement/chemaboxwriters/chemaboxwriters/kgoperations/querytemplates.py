from chemaboxwriters.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from chemaboxwriters.kgoperations.querykg import querykg
import json

def spec_inchi_query(inchi_string):
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER REGEX(str(?Inchi), REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(""" + '"' + inchi_string + '"' + """, "InChI=1S", "InChI=1"), "/t.+", ""), "/b.+", ""), "\\\\(", "\\\\\\\\("), "\\\\)", "\\\\\\\\)"), "i")
    }
    """
    #print(query)
    return query

def get_species_iri(inchi):
    #Query OntoSpecies to find Species IRI that corresponds to a given InChI.
    target = None
    results  = querykg(SPARQL_ENDPOINTS['ontospecies'], spec_inchi_query(inchi)) #query_endpoint(endpoint, spec_inchi_query(inchi))
    results = json.loads(results)
    if results:
        if 'speciesIRI' in results[0].keys():
            target = results[0]['speciesIRI']
    return target