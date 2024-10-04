from typing import Optional

from SPARQLWrapper import SPARQLWrapper, POST, JSON


class KgClient:
    QUERY_PREFIXES = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

PREFIX dabgeo:	  <http://www.purl.org/oema/infrastructure/>
PREFIX om:		  <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ict: 	  <http://ontology.eil.utoronto.ca/icontact.owl#>

PREFIX ocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>
PREFIX ocr: <http://www.theworldavatar.com/kg/ontocrystal/>
PREFIX zeo: <http://www.theworldavatar.com/kg/ontozeolite/>
PREFIX obe: <https://www.theworldavatar.com/kg/ontobuiltenv/>
PREFIX oplnrgl: <https://www.theworldavatar.com/kg/ontoplanningregulation/>
PREFIX oplt: <https://www.theworldavatar.com/kg/ontoplot/>
PREFIX ozng: <https://www.theworldavatar.com/kg/ontozoning/>"""

    def __init__(self, endpoint: str, user: Optional[str] = None, password: Optional[str] = None):
        sparql = SPARQLWrapper(endpoint)
        sparql.setReturnFormat(JSON)
        if user is not None and password is not None:
            sparql.setCredentials(user=user, passwd=password)
        sparql.setMethod(POST)
        self.sparql = sparql

    def query(self, query: str):
        if not query.startswith("PREFIX"):
            query = self.QUERY_PREFIXES + query

        self.sparql.setQuery(query)
        return self.sparql.queryAndConvert()
