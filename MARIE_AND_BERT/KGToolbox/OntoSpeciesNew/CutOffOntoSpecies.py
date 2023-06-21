import json
import os

from SPARQLWrapper import SPARQLWrapper, JSON, N3
# from Marie.Util.location import DATA_DIR


class CutOff:

    def __init__(self):
        self.endpoint_url = "http://www.theworldavatar.com/blazegraph"
        self.sparql_namespace = "copy_ontospecies_pubchem"
        self.ontology = "ontospecies_new"
        self.sub_ontology = "full"

    def query_blazegraph(self, query):
        print(f"Querying {self.endpoint_url}")
        sparql = SPARQLWrapper(f"{self.endpoint_url}/namespace/" + self.sparql_namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results

    def get_values_and_units(self):
        GET_VALUES_AND_UNITS = """
        CONSTRUCT { ?s <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value> ?o } 
        WHERE { ?s  rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 
                ?s  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value> ?o . }	
        """

    def get_all_properties(self):
        GET_ALL_PROPERTIES = """
        SELECT DISTINCT ?p 
        WHERE { 
        ?s  rdf:type  <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species> . 
        ?s  ?p  ?o }	
        """
        relation_iri_list = []
        rst = self.query_blazegraph(GET_ALL_PROPERTIES)["results"]["bindings"]
        for binding in rst:
            relation_iri = binding['p']['value']
            relation_iri_list.append(relation_iri)

        with open(os.path.join(DATA_DIR, "CrossGraph", self.ontology, self.sub_ontology, "species_relations.txt"),
                  "w") as f:
            f.write("\n".join(relation_iri_list))
            f.close()

    def run(self):
        FULL_QUERY_TEST = """
        CONSTRUCT { ?s ?p ?o } 
        WHERE {  ?s  ?p ?o . }	
        """
        rst = self.query_blazegraph(FULL_QUERY_TEST)

        with open("ontospecies.json", "w") as f:
            f.write(json.dumps(rst))
            f.close()


if __name__ == "__main__":
    cut_off = CutOff()
    cut_off.run()
