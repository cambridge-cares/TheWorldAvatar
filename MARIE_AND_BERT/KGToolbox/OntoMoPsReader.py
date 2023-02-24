import json
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
        self.sub_ontology = "numerical"
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology, self.sub_ontology)

        self.property_stop_list = ["Volume", "label", "imports", "hasNumericalValue",
                                   "hasUnit", "hasCBUFormula", "hasModularity", "value",
                                   "hasValue", "hasOuterCoordinationNumber", "type",
                                   "hasReferenceDOI", "hasMOPFormula", "hasCCDCNumber"]

        self.numerical_property_list = ["hasMolecularWeight", "hasCharge"]

        self.file_creator = IntegratedTrainingFileCreator(sparql_namespace="ontomops", ontology=self.ontology,
                                                          sub_ontology=self.sub_ontology,
                                                          endpoint_url="http://kg.cmclinnovations.com:81/blazegraph_geo",
                                                          same_frac=1.0, other_frac=0.1)

        self.query_blazegraph = self.file_creator.query_blazegraph
        self.SPARQL_TEMPLATE_WITH_PREFIX = """
          PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
          PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
          PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
          %s 
        """

    def collect_lateral_data(self):
        node_value_dict = {}
        triples = []
        numerical_triples = []
        LATERAL_DATA_QUERY = """
        PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT distinct ?mopIRI ?node ?value
        WHERE
        { 
          ?mopIRI rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#MetalOrganicPolyhedra>.
          ?mopIRI OntoSpecies:%s ?node .
          ?node Measure:hasValue ?valueIRI .
          ?valueIRI Measure:hasNumericalValue ?value .      
        }  
        """
        for numerical_property in self.numerical_property_list:
            QUERY = LATERAL_DATA_QUERY % numerical_property
            rst = self.file_creator.query_blazegraph(QUERY)["results"]["bindings"]
            for binding in rst:
                mopIRI = split_iri(binding["mopIRI"]["value"])
                node = split_iri(binding["node"]["value"])
                value = float(binding["value"]["value"])
                node_value_dict[node] = value
                triples.append((mopIRI, numerical_property, node))
                numerical_triples.append((mopIRI, numerical_property, value))
        return triples, numerical_triples, node_value_dict

    def filter_singular_nodes(self, triples):
        node_count_dict = {}
        singular_node_list = []
        non_singular_triples = []
        singular_triples = []
        numerical_triples = []

        for s, p, o in triples:
            self.file_creator.update_count_dict(node_count_dict, s)
            self.file_creator.update_count_dict(node_count_dict, o)

        for node in node_count_dict:
            if node_count_dict[node] == 1:
                singular_node_list.append(node)

        for s, p, o in triples:
            row = (s, p, o)

            if o in singular_node_list or s in singular_node_list:
                singular_triples.append(row)
            else:
                non_singular_triples.append(row)

        return singular_triples, non_singular_triples

    def query_all_triples(self):
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

    def get_all_triples(self):
        # 14348 triples in total
        # 1. split out the "singular" nodes
        cached_triple_path = os.path.join(self.full_dataset_dir, "cached_triples.json")
        if os.path.exists(cached_triple_path):
            # load the cached triples
            triples = json.loads(open(cached_triple_path).read())
        else:
            triples = self.query_all_triples()
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
        numerical_triples, numerical_values, node_value_dict = self.collect_lateral_data()
        all_triples += numerical_triples
        df_all = pd.DataFrame(all_triples)
        df_all.to_csv(f"{self.full_dataset_dir}/{self.sub_ontology}-train.txt", sep="\t", header=False, index=False)
        singular_triples, non_singular_triples = self.filter_singular_nodes(all_triples)
        df_singular = pd.DataFrame(singular_triples)

        non_singular_triples += numerical_triples
        df_non_singular = pd.DataFrame(non_singular_triples)
        df_singular.to_csv(f"{self.full_dataset_dir}/{self.sub_ontology}-singular-train.txt", sep="\t", header=False,
                           index=False)
        df_non_singular.to_csv(f"{self.full_dataset_dir}/{self.sub_ontology}-test.txt", sep="\t", header=False,
                               index=False)
        df_non_singular.to_csv(f"{self.full_dataset_dir}/{self.sub_ontology}-train-2.txt", sep="\t", header=False,
                               index=False)

        df_numerical = pd.DataFrame(numerical_values)
        df_numerical.to_csv(f"{self.full_dataset_dir}/numerical_eval.tsv", sep="\t", header=False,
                            index=False)

        # TODO: get the numerical stuff needed, use properties to filter them ...
        self.file_creator.create_supporting_files_for_embedding()
        with open(os.path.join(self.full_dataset_dir, "node_value_dict.json"), "w") as f:
            f.write(json.dumps(node_value_dict))
            f.close()


if __name__ == "__main__":
    my_reader = OntoMopsReader()
    my_reader.run()
