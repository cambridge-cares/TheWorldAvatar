import json
import os

from SPARQLWrapper import SPARQLWrapper, JSON

from KGToolbox.Tools import MakeIndex
from KGToolbox.Tools.CreateNegSamplingDictionary import NegSamplingCreator
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR


class IntegratedTrainingFileCreator:

    @staticmethod
    def update_count_dict(count_dict, key):
        if key in count_dict:
            count_dict[key] += 1
        else:
            count_dict[key] = 1

    def __init__(self, sparql_namespace, ontology, sub_ontology,
                 endpoint_url="http://www.theworldavatar.com/blazegraph", other_frac=0.0, same_frac=1.0,
                 node_value_dict=None):
        self.other_frac = other_frac
        self.same_frac = same_frac
        self.sparql_namespace = sparql_namespace
        self.endpoint_url = endpoint_url
        self.ontology = ontology
        self.sub_ontology = sub_ontology
        self.full_dataset_dir = os.path.join(DATA_DIR, "CrossGraph", self.ontology)
        self.sub_ontology_path = os.path.join(self.full_dataset_dir, self.sub_ontology)
        self.node_value_dict = node_value_dict
        self.cached_query_path = os.path.join(self.full_dataset_dir, "cached_query.json")
        if os.path.exists(self.cached_query_path):
            self.cached_query = json.loads(open(self.cached_query_path).read())
        else:
            self.cached_query = {}

    def query_blazegraph(self, query):
        # print(f"Querying {self.endpoint_url}")
        if query not in self.cached_query:
            sparql = SPARQLWrapper(f"{self.endpoint_url}/namespace/" + self.sparql_namespace + "/sparql")
            sparql.setQuery(query)
            sparql.setReturnFormat(JSON)
            results = sparql.query().convert()
            self.cached_query[query] = results
        else:
            results = self.cached_query[query]
        return results

    def filter_singular_nodes(self, triples):
        node_count_dict = {}
        singular_node_list = []
        non_singular_triples = []
        singular_triples = []
        numerical_triples = []

        for s, p, o in triples:
            self.update_count_dict(node_count_dict, s)
            self.update_count_dict(node_count_dict, o)

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

    def create_supporting_files_for_embedding(self, inference_target_dictionary=None, node_value_dict=None):
        ontology = f"{self.ontology}/{self.sub_ontology}"
        MakeIndex.create_indexing(self.sub_ontology, data_dir=f'CrossGraph/{ontology}')
        my_extractor = HopExtractor(dataset_dir=self.sub_ontology_path, dataset_name=self.sub_ontology)
        file_loader = FileLoader(full_dataset_dir=self.sub_ontology_path, dataset_name=self.sub_ontology)
        entity2idx, idx2entity, rel2idx, idx2rel = file_loader.load_index_files()
        if inference_target_dictionary:
            candidate_dict = self.create_inference_candidate_dict(entity2idx=entity2idx,
                                                                  target_dictionary=inference_target_dictionary)
            with open(f"{self.sub_ontology_path}/candidate_dict.json", "w") as f:
                f.write(json.dumps(candidate_dict))
                f.close()

        # where is the node_value_dict ?
        my_creator = NegSamplingCreator(dataset_dir=self.sub_ontology_path, ontology=self.sub_ontology,
                                        other_class_frac=self.other_frac, same_class_frac=self.same_frac,
                                        node_value_dict=node_value_dict)
        my_creator.create_full_neg_dictionary()
        return entity2idx

    def create_inference_candidate_dict(self, entity2idx, target_dictionary):
        """
        Make a dictionary mapping true tail to candidate entities in the form of indices
        1. create all uses list
        :return:
        """
        candidate_dict = {}
        role_list = list(target_dictionary.keys())
        role_list = list(set(role_list))
        role_list = [entity2idx[role] for role in role_list]
        for role_idx in role_list:
            candidate_dict[role_idx] = role_list
        return candidate_dict
