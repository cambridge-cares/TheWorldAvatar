import json
import os

import pandas as pd
import torch

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR


class ComplexInferenceDataset(torch.utils.data.Dataset):
    """

    """

    def __init__(self, df, full_dataset_dir, ontology, mode="train", inference=True, global_neg=False):
        super(ComplexInferenceDataset, self).__init__()
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.use_global_neg = global_neg
        print("Global neg", self.use_global_neg)
        if not self.use_global_neg:
            # Skip loading neg_sample_dict.json, which is not compatible with
            # older ontology training set (e.g. ontocompchem)
            self.neg_sample_dict_path = os.path.join(self.full_dataset_dir, "neg_sample_dict.json")
            self.neg_sample_dict = json.loads(open(self.neg_sample_dict_path).read())

        # if (inference):
        self.candidate_dict_path = os.path.join(self.full_dataset_dir, "candidate_dict.json")
        if os.path.exists(self.candidate_dict_path):
            self.candidate_dict = json.loads(open(self.candidate_dict_path).read())
            self.candidate_max = max([len(v) for k, v in self.candidate_dict.items()]) + 1
            print("max number of candidates is", self.candidate_max)

        self.node_value_dict_path = os.path.join(self.full_dataset_dir, "node_value_dict.json")
        if os.path.exists(self.node_value_dict_path):
            self.node_value_dict = json.loads(open(self.node_value_dict_path).read())
        else:
            self.node_value_dict = {}

        self.my_extractor = HopExtractor(
            dataset_dir=full_dataset_dir,
            dataset_name=ontology)
        self.mode = mode
        self.df = df
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.rel2idx.keys())
        self.use_cached_triples = False
        cached_triple_path = f"{self.full_dataset_dir}/triple_idx_{self.mode}.json"
        if os.path.exists(cached_triple_path) and self.use_cached_triples:
            # the triples are already cached
            self.triples = json.loads(open(cached_triple_path).read())
        else:
            if self.mode == "value_node":
                self.triples = self.create_value_node_triples()
            elif self.mode == "value_node_eval":
                self.triples = self.create_value_node_evaluation_triples()
            elif self.mode == "general_train":
                self.triples = self.create_triples_for_general_train()
            elif self.mode == "general_test":
                self.triples = self.create_triples_for_general_test()
            elif self.mode == "general_train_eval":
                self.triples = self.create_triples_for_general_test()
            else:
                self.triples = self.create_test_triples()
            if self.use_cached_triples:
                with open(cached_triple_path, "w") as f:
                    f.write(json.dumps(self.triples))
                    f.close()

    def create_value_node_evaluation_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            if row[2] in self.node_value_dict:
                # if row[1] in self.p_stop_list:
                tail_all = range(0, self.ent_num)
                for tail in tail_all:
                    triples.append((s, p, tail, o))
        return triples

    def create_value_node_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            # if row[1] in self.p_stop_list:
            if row[2] in self.node_value_dict:
                triples.append((s, p, o))
        return triples

    def load_numerical_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            # print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            v = float(row[2])
            true_triple = (s, p, v)
            triples.append(true_triple)

        return triples

    def create_fake_triple(self, s, p, o, mode="head"):
        s_p_str = f'{s}_{p}'
        if s_p_str in self.neg_sample_dict and o not in self.node_value_dict:
            fake_candidates = self.neg_sample_dict[s_p_str]
        else:
            fake_candidates = [0]
        return fake_candidates

    def create_triples_for_general_train(self):
        counter = 0
        triples = []
        for idx, row in self.df.iterrows():
            counter += 1
            if counter % 10000 == 0:
                print(f"{counter} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            if row[2] in self.node_value_dict or row[2].strip() == "hasInstance":
                v = float(self.node_value_dict[row[2]])
                candidates = [0]
            else:
                v = -999
                if self.use_global_neg:
                    candidates = range(0, self.ent_num)
                else:
                    candidates = self.create_fake_triple(s, p, o)

            true_triple = (s, p, o, 1, v)

            for i in candidates:
                triple_idx_string = f"{s}_{p}_{i}"
                if not self.my_extractor.check_triple_existence(triple_idx_string):
                    fake_triple = (s, p, i, 0, -999)
                    triples.append(true_triple)
                    triples.append(fake_triple)

        print(f"Total number of triples for training {len(triples)}")
        return triples

    def create_triples_for_general_test(self):
        print(f"using global neg test {self.use_global_neg}")
        triples = []
        progress_counter = 0
        for idx, row in self.df.iterrows():
            progress_counter += 1
            if progress_counter % 10000 == 0:
                print(f"{progress_counter} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            if row[2] not in self.node_value_dict:
                triples.append((s, p, o, o))
                # check o and remove any o with
                if self.use_global_neg:
                    candidates = range(0, self.ent_num)
                    padding_num = self.ent_num
                else:
                    candidates = list(set(self.my_extractor.extract_neighbour_from_idx(str(s))))
                    padding_num = self.ent_num

                fake_triples_list = []
                for i in candidates:
                    triple_idx_string = f"{s}_{p}_{i}"
                    if not self.my_extractor.check_triple_existence(triple_idx_string):
                        # also filter out the numerical triples
                        fake_triples_list.append((s, p, i, o))
                        # counter += 1
                triples += fake_triples_list
                length_diff = padding_num - len(fake_triples_list) - 1
                padding_list = [(s, p, -1, o)] * length_diff
                triples += padding_list
                if length_diff < 0:
                    print("We have a problem", length_diff, len(candidates))
        return triples

    def create_test_triples(self):
        triples = []
        for idx, test_row in self.df.iterrows():
            if idx % 10000 == 0:
                print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[test_row[0]]
            p = self.rel2idx[test_row[1]]
            o = self.entity2idx[test_row[2]]
            # if test_row[1] == "hasRole":
            candidates = self.candidate_dict[str(o)]
            length_diff = self.candidate_max - len(candidates)
            padding_list = [-1] * length_diff
            candidates += padding_list
            for tail in candidates:
                triple_idx_string = f"{s}_{p}_{tail}"
                if o == tail:
                    triples.append((s, p, tail, o))
                elif not self.my_extractor.check_triple_existence(triple_idx_string):
                    triples.append((s, p, tail, o))
                else:
                    triples.append((s, p, -1, o))
        return triples

    def __len__(self):
        return len(self.triples)

    def __getitem__(self, item):
        return self.triples[item]


