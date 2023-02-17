import os

import pandas as pd

from KGToolbox.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from Marie.Util.location import DATA_DIR


def split_iri(iri):
    if "#" in iri:
        return iri.split("#")[-1]
    elif "/" in iri:
        return iri.split("/")[-1]
    else:
        return iri


class OntoMopsReader:
    """
    This class handles all tasks for creating the training set of OntoMoPs ontology
    """

    def __init__(self):
        # http://kg.cmclinnovations.com:81/blazegraph_geo/#query
        self.ontology = "OntoMoPs"
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)

        self.property_stop_list = ["Volume", "label", "imports", "hasNumericalValue",
                                   "hasUnit", "hasCBUFormula", "hasModularity", "value",
                                   "hasValue", "hasSymbol", "hasOuterCoordinationNumber",
                                   "hasReferenceDOI", "hasMOPFormula", "hasCCDCNumber"]

        self.file_creator = IntegratedTrainingFileCreator(sparql_namespace="ontomops",
                                                          endpoint_url="http://kg.cmclinnovations.com:81/blazegraph_geo")

        self.query_blazegraph = self.file_creator.query_blazegraph
        self.SPARQL_TEMPLATE_WITH_PREFIX = """
          PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
          PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
          PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
          %s 
        """

    def update_count_dict(self, count_dict, key):
        if key in count_dict:
            count_dict[key] += 1
        else:
            count_dict[key] = 1

    def collect_lateral_data(self):
        mop_formula_dict = self.get_mop_formula()

    def filter_singular_nodes(self, triples):
        node_count_dict = {}
        singular_node_list = []
        for s, p, o in triples:
            self.update_count_dict(node_count_dict, s)
            self.update_count_dict(node_count_dict, o)

        for node in node_count_dict:
            if node_count_dict[node] == 1:
                print(node)
                singular_node_list.append(node)

    def get_all_triples(self):
        # 14348 triples in total
        # 1. split out the "singular" nodes
        triples = []
        QUERY_GET_ALL_TRIPLES = """
        SELECT ?s ?p ?o  
        WHERE {
            ?s ?p ?o  .
        }
        """
        rst = self.query_blazegraph(QUERY_GET_ALL_TRIPLES)["results"]["bindings"]
        for binding in rst:
            s = split_iri(binding["s"]["value"])
            p = split_iri(binding["p"]["value"])
            o_type = binding["o"]["type"]
            if o_type == "literal":
                o = binding["o"]["value"]
            elif o_type:
                o = split_iri(binding["o"]["value"])

            else:
                o = binding["o"]
                print("EXCEPTION", o_type, o)
            if p not in self.property_stop_list:
                triples.append((s, p, o))

        return triples

    def get_mop_formula(self):
        mop_formula_dict = {}
        QUERY_MOP_FORMULA = self.SPARQL_TEMPLATE_WITH_PREFIX % """
          SELECT ?mopIRI ?MOPFormula 
          WHERE {
            ?mopIRI OntoMOPs:hasMOPFormula ?MOPFormula .
          }
        """
        rst = self.query_blazegraph(QUERY_MOP_FORMULA)["results"]["bindings"]
        for binding in rst:
            mopIRI = binding["mopIRI"]["value"].split("/")[-1]
            formula = binding["MOPFormula"]["value"]
            mop_formula_dict[mopIRI] = formula
        return mop_formula_dict

    def run(self):
        all_triples = self.get_all_triples()
        df_all = pd.DataFrame(all_triples)
        df_all.to_csv(f"{self.full_dataset_dir}/{self.ontology}-train.tsv", sep="\t", header=False, index=False)
        self.filter_singular_nodes(all_triples)


if __name__ == "__main__":
    my_reader = OntoMopsReader()
    my_reader.run()
