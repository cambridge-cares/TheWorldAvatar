import json
import os
import random

from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader


class NegSamplingCreator:
    """
    NegSamplingCreator creates a dictionary mapping all s_p combination to all neighbouring
    entities of s that are fake triples ...
    """

    @staticmethod
    def random_sample_by_fraction(dataset, frac):
        return random.sample(dataset, int(len(dataset) * frac))

    def __init__(self, dataset_dir, ontology, other_class_frac=0.0, same_class_frac=1.0, node_value_dict=None, allowed_relations = None):
        self.other_class_frac = other_class_frac
        self.same_class_frac = same_class_frac
        self.dataset_dir = dataset_dir
        self.ontology = ontology
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.neighbour_dictionary = self.load_neighbour_dictionary()
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.triples = self.file_loader.load_all_triples()
        self.hop_extractor = HopExtractor(dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.node_value_dict = node_value_dict
        self.allowed_relations = allowed_relations

    def load_neighbour_dictionary(self):
        dict_path = os.path.join(DATA_DIR, self.dataset_dir, "three_hop_dict_index")
        return json.loads(open(dict_path).read())

    def create_full_neg_dictionary(self):
        """
        For each subject, create a universal neg dictionary, use neighbour extract to check whether the triple existis
        :return:
        """
        # stop_p_list = ["hasLogP", "hasDensity", "hasBoilingPoint",
        #                "hasSolubility", "hasLogP", "hasLogS", "hasMolecularWeight",
        #                "hasMeltingPoint"]
        # create the list of tails first

        # avoid put numerical nodes into neg sample

        s_p_neg_dict = {}
        s_p_pos_dict = {}
        all_entities = []
        p_list = []
        p_all_dict = {}

        numerical_nodes = set(list(self.node_value_dict.keys()))
        neighbour_dictionary = {}
        counter = 0
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            if o not in numerical_nodes:
                s_idx = self.entity2idx[s]
                o_idx = self.entity2idx[o]
                all_entities.append(s_idx)
                all_entities.append(o_idx)
                p_idx = self.rel2idx[p]
                p_idx_str = str(p_idx)
                if p_idx_str in p_all_dict:
                    p_all_dict[p_idx_str].append(o_idx)
                else:
                    p_all_dict[p_idx_str] = [o_idx]
                s_p_idx_str = f"{s_idx}_{p_idx}"
                s_p_neg_dict[s_p_idx_str] = []
                if s_p_idx_str not in s_p_pos_dict:
                    s_p_pos_dict[s_p_idx_str] = [o_idx]
                else:
                    s_p_pos_dict[s_p_idx_str].append(o_idx)

                if str(p_idx) not in p_list:
                    p_list.append(str(p_idx))

        all_entities = list(set(all_entities))
        total_neg_number = 0
        for s_p_idx in s_p_pos_dict:
            p_idx = s_p_idx.split("_")[1]
            all_pos_entities = s_p_pos_dict[s_p_idx]
            all_neg_entities_different_class = list(set(all_entities) - set(all_pos_entities) - set(p_all_dict[p_idx]))
            all_neg_entities_same_class = list(set(p_all_dict[p_idx]) - set(all_pos_entities))
            # print(len(all_neg_entities_same_class))
            # print(len(all_neg_entities_different_class))
            # 0.1, 0.5 so far best
            # test 0.05, 0.5         # 0.5, 0.05 , 6430992
            # test 0.02, 0.2         # 0.2, 0.02 , 2568773
            sampled_neg_entities_different_class = self.random_sample_by_fraction(all_neg_entities_different_class,
                                                                                  frac=self.other_class_frac)
            sampled_neg_entities_same_class = self.random_sample_by_fraction(all_neg_entities_same_class,
                                                                             frac=self.same_class_frac)
            # print("--------------------------")
            # print(len(sampled_neg_entities_different_class))
            # print(len(sampled_neg_entities_same_class))
            sampled_eng_entities = list(set(sampled_neg_entities_same_class + sampled_neg_entities_different_class))
            # print(len(sampled_eng_entities))
            # print("=====================")
            total_neg_number += len(sampled_eng_entities)

            # use a finer strategy to do the neg sampling
            # 1. get all

            s_p_neg_dict[s_p_idx] = sampled_eng_entities
            # print(len(all_entities))
            # print(len(set(all_pos_entities)))
            # print(len(set(all_neg_entities)))
            # print("-------")
        print("=========================================")
        print("total neg sample number", total_neg_number)

        with open(os.path.join(self.full_dataset_dir, "neg_sample_dict.json"), "w") as f:
            f.write(json.dumps(s_p_neg_dict))
            f.close()
        with open(os.path.join(self.full_dataset_dir, "p_all_dict.json"), "w") as f:
            f.write(json.dumps(p_all_dict))
            f.close()

    def run(self):
        """
        :return: a index dictionary of s_p -> O = {o_1, ..., o_n}
        """
        # create the list of tails first
        s_p_neg_dict = {}
        tail_list = []
        head_list = []
        p_list = []
        neighbour_dictionary = {}
        counter = 0
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            s_idx = self.entity2idx[s]
            # o_idx = self.entity2idx[o]
            p_idx = self.rel2idx[p]
            s_p_idx_str = f"{s_idx}_{p_idx}"
            s_p_neg_dict[s_p_idx_str] = []
            if str(p_idx) not in p_list:
                p_list.append(str(p_idx))

        counter = 0
        for triple in self.triples:
            counter += 1
            print(f"{counter} out of {len(self.triples)}")
            s, p, o = [e.strip() for e in triple.split("\t")]
            s_idx = self.entity2idx[s]
            o_idx = self.entity2idx[o]
            p_idx = self.rel2idx[p]
            # put the o in the s_other_p list
            for other_p in p_list:
                if other_p != p:
                    s_p_idx_str = f"{s_idx}_{other_p}"
                    if s_p_idx_str in s_p_neg_dict:
                        s_p_neg_dict[s_p_idx_str].append(o_idx)
                    else:
                        s_p_neg_dict[s_p_idx_str] = [o_idx]

        with open(os.path.join(self.full_dataset_dir, "neg_sample_dict.json"), "w") as f:
            f.write(json.dumps(s_p_neg_dict))
            f.close()


if __name__ == "__main__":
    _ontology = "role_only"
    _dataset_dir = f"CrossGraph/ontospecies_new/role_only"
    my_creator = NegSamplingCreator(dataset_dir=_dataset_dir, ontology=_ontology)
    my_creator.create_full_neg_dictionary()
