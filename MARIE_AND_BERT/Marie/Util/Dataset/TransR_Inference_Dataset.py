import json
import os

import pandas as pd
import torch

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR


class TransRInferenceDataset(torch.utils.data.Dataset):
    """

    """

    def __init__(self, df, full_dataset_dir, ontology, mode="train", inference=True):
        super(TransRInferenceDataset, self).__init__()
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.neg_sample_dict_path = os.path.join(self.full_dataset_dir, "neg_sample_dict.json")
        self.neg_sample_dict = json.loads(open(self.neg_sample_dict_path).read())
        
        if(inference): 
            self.candidate_dict_path = os.path.join(self.full_dataset_dir, "candidate_dict.json")
            self.candidate_dict = json.loads(open(self.candidate_dict_path).read())
            self.candidate_max = max([len(v) for k, v in self.candidate_dict.items()])
            self.node_value_dict_path = os.path.join(self.full_dataset_dir, "node_value_dict.json")
            self.node_value_dict = json.loads(open(self.node_value_dict_path).read())
            self.p_stop_list = ["hasLogP", "hasDensity", "hasBoilingPoint",
                            "hasSolubility", "hasLogP", "hasLogS", "hasMolecularWeight",
                            "hasMeltingPoint"]

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
            if self.mode == "train":
                self.triples = self.create_all_triples()
            elif self.mode == "numerical":
                self.triples = self.load_numerical_triples()
            elif self.mode == "train_eval":
                self.triples = self.create_train_small_triples_for_evaluation()
            elif self.mode == "value_node":
                self.triples = self.create_value_node_triples()
            elif self.mode == "value_node_eval":
                self.triples = self.create_value_node_evaluation_triples()
            elif self.mode == "agent_train":
                self.triples = self.create_triples_for_agent_train()
            elif self.mode == "agent_test":
                self.triples = self.create_triples_for_agent_test()
            elif self.mode == "agent_train_eval":
                self.triples = self.create_triples_for_agent_test()
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
            if row[1] in self.p_stop_list:
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
            if row[1] in self.p_stop_list:
                triples.append((s, p, o))
        return triples




    def create_train_small_triples_for_evaluation(self):
        triples = []
        # tail_all = range(0, self.ent_num)
        # instead of random tail all, find tails under the same class
        counter = 0
        for idx, row in self.df.iterrows():
            counter += 1
            # print(f"Small train triples: {counter} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            if row[1] == "hasRole":
                candidates = self.candidate_dict[str(o)]
                length_diff = self.candidate_max - len(candidates)
                padding_list = [-1] * length_diff
                candidates += padding_list
                for tail in candidates:  # tail_all = self
                    # if tail_all:
                    #     for tail in tail_all:
                    triple_idx_string = f"{s}_{p}_{tail}"
                    if o == tail:
                        triples.append((s, p, tail, o))
                    elif not self.my_extractor.check_triple_existence(triple_idx_string):
                        triples.append((s, p, tail, o))
                    else:
                        triples.append((s, p, -1, o))
        return triples

    def load_numerical_triples(self):
        triples = []
        for idx, row in self.df.iterrows():
            # print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            v = float(row[2])
            if 0 < v < 500:
                true_triple = (s, p, v)
                triples.append(true_triple)
        return triples

    def create_fake_triple(self, s, p, o, mode="head"):
        s_p_str = f'{s}_{p}'
        fake_candidates = self.neg_sample_dict[s_p_str]
        return fake_candidates
    
    def create_triples_for_agent_train(self):
        triples=[]
        for idx, row in self.df.iterrows():
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            true_triple = (s,p,o,-999)
            for i in range(0, self.ent_num):
                triple_idx_string = f"{s}_{p}_{i}"
                if not self.my_extractor.check_triple_existence(triple_idx_string):
                    fake_triple = (s,p,i,-999)
                    triples.append((true_triple, fake_triple))
        return triples

    def create_triples_for_agent_test(self):
        triples=[]
        for idx, row in self.df.iterrows():
            counter=0
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            triples.append((s,p,o,-999))
            for i in range(self.ent_num):
                triple_idx_string = f"{s}_{p}_{i}"
                if not self.my_extractor.check_triple_existence(triple_idx_string):
                    triples.append((s,p,i,-999))
                    counter+=1
            length_diff = (self.ent_num -1) - counter
            for i in range(length_diff):
                triples.append((s,p,-1,-999))

        return triples


    def create_test_triples(self):
        triples = []
        for idx, test_row in self.df.iterrows():
            # print(f"{idx} out of {len(self.df)}")
            s = self.entity2idx[test_row[0]]
            p = self.rel2idx[test_row[1]]
            o = self.entity2idx[test_row[2]]
            if test_row[1] == "hasRole":
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

    def create_all_triples(self):
        triples = []
        counter = 0
        for idx, row in self.df.iterrows():
            counter += 1
            # print(f"Train triples {counter} out of {len(self.df)}")
            s = self.entity2idx[row[0]]
            p = self.rel2idx[row[1]]
            o = self.entity2idx[row[2]]
            if row[2] in self.node_value_dict:
                value = float(self.node_value_dict[row[2]])
                if 0 < value < 500:
                    # if True:
                    true_triple = (s, p, o, value)
                    fake_triple = (s, p, o)
                    triples.append((true_triple, fake_triple))
            else:
                if row[1] not in self.p_stop_list:
                    true_triple = (s, p, o, -999)
                    fake_tails = self.create_fake_triple(s, p, o, mode="head")
                    for fake_tail in fake_tails:
                        fake_triple = (s, p, fake_tail)
                        triples.append((true_triple, fake_triple))
        return triples

    def create_inference_test_sample(self, reaction_triples):
        """
        Given the triples for test, we try the following evaluation, use the reactants embeddings r to calculate the
        embedding of the reaction R' which is not observed in the training set.
        Then use R' to infer the products ...
        Then reverse the process to infer reactants given products ...
        :return: list of tuples (S_r1, isReactant), (S_r2, isReactant) -> (S_p1), (S_p2)
        """
        triples = []
        for reaction in reaction_triples:
            data = reaction_triples[reaction]
            r_idx_list = [self.entity2idx[r] for r in data['reactants']]
            p_idx_list = [self.entity2idx[p] for p in data['products']]
            triples.append((r_idx_list, p_idx_list))
        return triples

    def __len__(self):
        return len(self.triples)

    def __getitem__(self, item):
        return self.triples[item]


if __name__ == "__main__":
    sub_ontology = "role_with_subclass_full_attributes_with_class_flatten"
    full_dir = os.path.join(DATA_DIR, 'CrossGraph', f'ontospecies_new/{sub_ontology}')
    batch_size = 32
    # ===================================================================================================
    df_train = pd.read_csv(os.path.join(full_dir, f"{sub_ontology}-train-2.txt"), sep="\t", header=None)
    df_train_small = df_train.sample(frac=0.01)
    df_test = pd.read_csv(os.path.join(full_dir, f"{sub_ontology}-test.txt"), sep="\t", header=None)
    df_numerical = pd.read_csv(os.path.join(full_dir, f"numerical_eval.tsv"), sep="\t", header=None)
    # ===================================================================================================
    # train_set = TransRInferenceDataset(df_train, full_dataset_dir=full_dir, ontology=sub_ontology, mode="train")
    # test_set = TransRInferenceDataset(df_test, full_dataset_dir=full_dir, ontology=sub_ontology, mode="test")
    # train_eval_set = TransRInferenceDataset(df_train_small, full_dataset_dir=full_dir, ontology=sub_ontology,
    #                                         mode="train_eval")
    # numerical_set = TransRInferenceDataset(df_numerical, full_dataset_dir=full_dir, ontology=sub_ontology,
    #                                        mode="numerical")

    # test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=test_set.candidate_max, shuffle=False)
    # train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=32, shuffle=True)
    # numerical_dataloader = torch.utils.data.DataLoader(numerical_set, batch_size=32, shuffle=True)
    # train_eval_set_dataloader = torch.utils.data.DataLoader(train_eval_set, batch_size=train_eval_set.candidate_max,
    #                                                         shuffle=False)

    # value_node_set = TransRInferenceDataset(df=df_train, full_dataset_dir=full_dir,
    #                                         ontology=sub_ontology,
    #                                         mode="value_node")
    # dataloader_value_node = torch.utils.data.DataLoader(value_node_set, batch_size=batch_size, shuffle=False)
    # for triple in dataloader_value_node:
    #     print(triple)

    # value_node_eval_set = TransRInferenceDataset(df=df_train, full_dataset_dir=full_dir,
    #                                              ontology=sub_ontology,
    #                                              mode="value_node_eval")

    # dataloader_value_node_eval = torch.utils.data.DataLoader(value_node_eval_set, batch_size=value_node_eval_set.ent_num, shuffle=False)
    # for row in dataloader_value_node_eval:
    #     print(row)