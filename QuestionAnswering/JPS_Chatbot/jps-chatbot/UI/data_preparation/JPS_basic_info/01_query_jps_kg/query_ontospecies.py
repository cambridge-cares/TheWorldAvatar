import json
import os

from location import TRAINING_FILES_DIR

from SPARQLWrapper import SPARQLWrapper, JSON


def fire_query(query):
    namespace = "ontospecies"
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/" + namespace + "/sparql")
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()
    return results


TEST_ONTOSPECIES_IDENTIFIER_QUERY = '''
PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT (GROUP_CONCAT (DISTINCT ?label; separator="; ") AS ?labels)
	   (GROUP_CONCAT (DISTINCT ?alt_label; separator="; ") AS ?alt_labels)
	   (GROUP_CONCAT (DISTINCT ?smile; separator="; ") AS ?smiles)
	   (GROUP_CONCAT (DISTINCT ?inChI; separator="; ") AS ?inChIs)
	   (GROUP_CONCAT (DISTINCT ?formula; separator="; ") AS ?formulas)
 
WHERE {
		
<http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000> rdfs:label          							?label .
<http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000> skos:altLabel       							?alt_label .
<http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000> ontospecies:SMILES  							?smile . 
<http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000> ontospecies:inChI   							?inChI  .
<http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000> ontospecies:hasMolecularFormula 			    ?formularNode . 
?formularNode rdfs:label ?formula .
  
}  
'''


ONTOSPECIES_IDENTIFIER_QUERY = '''
PREFIX ontospecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT  ?species

	   (GROUP_CONCAT (DISTINCT ?label; separator="; ") AS ?labels)
	   (GROUP_CONCAT (DISTINCT ?alt_label; separator="; ") AS ?alt_labels)
	   (GROUP_CONCAT (DISTINCT ?smile; separator="; ") AS ?smiles)
	   (GROUP_CONCAT (DISTINCT ?inChI; separator="; ") AS ?inChIs)
	   (GROUP_CONCAT (DISTINCT ?formula; separator="; ") AS ?formulas)


WHERE {
		
  ?species rdf:type  ontospecies:Species ;
           rdfs:label          							?label ;
  
  OPTIONAL {
           
  ?species skos:altLabel       							?alt_label ;
      ontospecies:SMILES  							?smile ; 
  ontospecies:inChI   							?inChI  ;
  ontospecies:hasMolecularFormula 			    ?formularNode . 
  ?formularNode rdfs:label ?formula .
  
    }
}  GROUP BY ?species  
'''

TEST_IDENTIFIERS = fire_query(TEST_ONTOSPECIES_IDENTIFIER_QUERY)
print(TEST_IDENTIFIERS)


REAL_IDENTIFIERS = fire_query(ONTOSPECIES_IDENTIFIER_QUERY)


with open(os.path.join(TRAINING_FILES_DIR, 'ONTOSPECIES_IDENDITIFERS_QUERY_RESULT'), 'w') as f:
    f.write(json.dumps(REAL_IDENTIFIERS))
    f.close()
