import json
import os

import pandas as pd

from KGToolbox.Tools.IntegratedTrainingFileCreator import IntegratedTrainingFileCreator
from KGToolbox.Tools.GeneralTools import split_iri
from Marie.Util.location import DATA_DIR




class OntoMopsReader:
    """
    This class handles all tasks for creating the training set of OntoMoPs ontology
    """

    def __init__(self, sub_ontology):
        # http://kg.cmclinnovations.com:81/blazegraph_geo/#query
        self.ontology = "OntoMoPs"
        self.sub_ontology = sub_ontology
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology, self.sub_ontology)

        self.property_stop_list = ["Volume", "label", "imports", "hasNumericalValue",
                                   "hasUnit", "hasCBUFormula", "hasModularity", "value",
                                   "hasValue", "hasOuterCoordinationNumber", "type",
                                   "hasReferenceDOI", "hasMOPFormula", "hasCCDCNumber"]

        self.reversed_properties_list = ["isFunctioningAs", "hasAssemblyModel", "hasChemicalBuildingUnit"]

        self.numerical_property_list = ["hasMolecularWeight", "hasCharge"]

        self.file_creator = IntegratedTrainingFileCreator(sparql_namespace="ontomops", ontology=self.ontology,
                                                          sub_ontology=self.sub_ontology,
                                                          endpoint_url="http://kg.cmclinnovations.com:81/blazegraph_geo",
                                                          same_frac=1.0, other_frac=1.0)

        self.query_blazegraph = self.file_creator.query_blazegraph
        self.filter_singular_nodes = self.file_creator.filter_singular_nodes
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
                if p not in self.reversed_properties_list:
                    triples.append((s, p, o))
                else:
                    p = p + "Reversed"
                    triples.append((o, p, s))
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

    def get_implicit_triples(self):

        QUERY_CUB_TO_AM = """
        PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT DISTINCT  ?sub ?obj 
        WHERE
        {   
        ?sub rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#ChemicalBuildingUnit>.
        ?mop OntoMOPs:hasChemicalBuildingUnit ?sub.
        ?mop OntoMOPs:hasAssemblyModel ?obj.
        }
        GROUP BY ?sub ?obj
        """

        QUERY_SHAPE_TO_AM = """
        PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT distinct ?sub ?obj
        WHERE
        {   
          ?shape OntoMOPs:hasSymbol ?sub .
          ?obj OntoMOPs:hasPolyhedralShape ?shape.
          ?mopIRI OntoMOPs:hasAssemblyModel ?obj.
        }
        """

        QUERY_POINT_TO_MOP = """
        PREFIX OntoMOPs: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX Measure: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT distinct ?sub ?obj
        WHERE
        { 
          ?am rdf:type <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#AssemblyModel>.
          ?am OntoMOPs:hasSymmetryPointGroup ?sub . 
          ?obj OntoMOPs:hasAssemblyModel ?am.
        }
        """
        query_list = [QUERY_CUB_TO_AM, QUERY_SHAPE_TO_AM, QUERY_POINT_TO_MOP]
        implicit_relations = ["cbuToAM", "shapeToAM", "pointToMoP"]
        triples = []
        for query, relation in zip(query_list, implicit_relations):
            rst = self.query_blazegraph(query)["results"]["bindings"]
            for binding in rst:
                sub = self.classify_and_shorten_node(binding["sub"])
                obj = self.classify_and_shorten_node(binding["obj"])
                triples.append((sub, relation, obj))

        return triples


    def classify_and_shorten_node(self, node):
        node_type = node["type"]
        if node_type == "literal":
            return node["value"]
        elif node_type == "uri":
            return split_iri(node["value"])
        else:
            return node["value"]

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
        implicit_triples = self.get_implicit_triples()
        numerical_triples, numerical_values, node_value_dict = self.collect_lateral_data()
        all_triples += numerical_triples
        all_triples += implicit_triples

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

        with open(os.path.join(self.full_dataset_dir, "node_value_dict.json"), "w") as f:
            f.write(json.dumps(node_value_dict))
            f.close()
        self.file_creator.create_supporting_files_for_embedding(node_value_dict=node_value_dict)

    def test(self):
        self.get_implicit_triples()


if __name__ == "__main__":
    my_reader = OntoMopsReader(sub_ontology="numerical_with_implicit")
    my_reader.run()
