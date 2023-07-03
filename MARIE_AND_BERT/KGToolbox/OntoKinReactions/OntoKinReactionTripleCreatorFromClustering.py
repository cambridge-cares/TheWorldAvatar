import hashlib
import json
import os
import random
import re
from pprint import pprint

import pandas as pd

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR


class OntoKinReactionTripleCreatorFromClustering:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)

        self.reaction_equation_dict_path = os.path.join(self.full_dataset_dir, "ontokin_reactions_value_dict.json")
        self.reaction_equation_dict = json.loads(open(self.reaction_equation_dict_path).read())
        self.species_hash_dict = {}

    def extract_subgraph_based_on_reactions_clustering(self, eps, min_sample):
        # TODO: load clustering results, create the key based on the eps and min_sample
        entity2idx, idx2entity, rel2idx, idx2rel = self.file_loader.load_index_files()
        all_selected_train_reactions = []
        clustering_result_path = os.path.join(self.full_dataset_dir, "sample/cluster_result.json")
        clustering_result = json.loads(open(clustering_result_path).read())
        selected_reaction_idx_list = json.loads(
            open(f"{self.full_dataset_dir}/sample/sampled_reaction_idx_list.json").read())
        selected_clustering_key = f"{str(eps)}_{str(min_sample)}"
        selected_clustering = clustering_result[selected_clustering_key]
        expected_length = selected_clustering['count_dict']['0']
        reaction_idx_list_from_clustering = selected_clustering['reaction_idx_list']
        print("expected length of selected reactions", expected_length)
        for idx, cluster_label in enumerate(reaction_idx_list_from_clustering):
            if cluster_label == 0:
                print(idx, cluster_label)
                selected_reaction_idx = selected_reaction_idx_list[idx]
                selected_reaction_iri = idx2entity[selected_reaction_idx]
                equation = self.reaction_equation_dict[selected_reaction_iri]
                all_selected_train_reactions.append(selected_reaction_iri)
        all_selected_test_reactions = random.sample(all_selected_train_reactions, 10)
        all_selected_train_reactions = list(set(all_selected_train_reactions) - set(all_selected_test_reactions))
        return all_selected_train_reactions, all_selected_test_reactions

    def create_sampled_triples_for_inference(self, triple, selected_reaction_idx_train_list,
                                             selected_reaction_idx_test_list):
        # TODO: iterate through the triples, select triples with selected reactions
        pass

    def create_triples(self, reaction_dict):
        all_triples = []
        species_hash_dict = {}
        reaction_reactant_products_dict = {}
        for equation, row in reaction_dict.items():
            reactants, products, reaction_iri = row["reactants"], row["products"], row["reaction_iri"]
            reactant_triples = []
            product_triples = []
            reactants_in_reactions = []
            products_in_reactions = []
            for r in reactants:
                r_hash = self.make_hash(r)
                species_hash_dict[r_hash] = r
                reactants_in_reactions.append(r_hash)
                reactant_triples.append((r_hash, "isReactant", reaction_iri))
                reactant_triples.append((reaction_iri, "hasReactant", r_hash))
            for p in products:
                p_hash = self.make_hash(p)
                species_hash_dict[p_hash] = p
                products_in_reactions.append(p_hash)
                product_triples.append((p_hash, "isProduct", reaction_iri))
                product_triples.append((reaction_iri, "hasProduct", p_hash))
            reaction_reactant_products_dict[reaction_iri] = {"reactants": reactants_in_reactions,
                                                             "products": products_in_reactions}
            all_triples += (reactant_triples + product_triples)

        return all_triples, species_hash_dict, reaction_reactant_products_dict

    def make_hash(self, value):
        encoded_value = value.encode('utf-8')
        species_hash = str(hashlib.sha256(encoded_value).hexdigest())
        self.species_hash_dict[species_hash] = value
        return species_hash


if __name__ == "__main__":
    my_creator = OntoKinReactionTripleCreatorFromClustering()
    my_creator.create_triples()
