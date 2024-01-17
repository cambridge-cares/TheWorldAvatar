import os, sys

sys.path.append("../..")
import random

import pandas as pd
import torch

from Marie.Util.Models.TransEAScoreModel import TransEAScoreModel
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.location import DATA_DIR
from Marie.Util.CommonTools.FileLoader import FileLoader


class EmbeddingTest:

    def __init__(self):
        self.ontology = "OntoMoPs"
        self.dataset_dir = "CrossGraph/OntoMoPs"
        self.dataset_name = "numerical_with_implicit"
        if self.dataset_name is not None:
            self.dataset_dir = os.path.join(self.dataset_dir, self.dataset_name)

        self.file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                      dataset_name=self.dataset_name)
        self.device = "cpu"
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()

        self.value_dictionary = self.file_loader.load_value_dict(dict_type="json",
                                                                 file_name="node_value_dict.json")

        self.score_model = TransEAScoreModel(device=self.device,
                                             dataset_dir=self.dataset_dir, dim=50)
        self.score_model.load_model(f"bert_{self.ontology}_test")
        self.score_model = self.score_model.to(self.device)
        self.largest = False
        self.subgraph_extractor = HopExtractor(dataset_dir=self.dataset_dir, dataset_name=self.dataset_name,
                                               numerical=False)
        # self.all_heads = self.file_loader.load_all_heads_tensor()

    def test_triple_distance(self, test_entity, test_rel):
        # Q1884806	P2102	ff4f077428e5023035faa1cd7971e5afae17b707
        rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                    header=None)
        ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'ent_embedding.tsv'), sep='\t',
                                    header=None)

        attr_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'attr_embedding.tsv'), sep='\t',
                                     header=None)

        bias_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'bias_embedding.tsv'), sep='\t',
                                     header=None)

        relation_idx = self.rel2idx[test_rel]
        rel = torch.tensor(rel_embedding.iloc[relation_idx].values).to(self.device)
        attr = torch.tensor(attr_embedding.iloc[relation_idx].values).to(self.device)
        bias = torch.tensor(bias_embedding.iloc[relation_idx].values).to(self.device)
        head_idx = self.entity2idx[test_entity]
        heads_idx = torch.Tensor([head_idx])
        tails_idx = torch.Tensor(self.subgraph_extractor.extract_neighbour_from_idx(head_idx))
        head = torch.tensor(ent_embedding.iloc[heads_idx].values).to(self.device)
        tails = torch.tensor(ent_embedding.iloc[tails_idx].values).to(self.device)
        repeat_num = len(tails)
        heads = head.repeat(repeat_num, 1).to(self.device)
        rels = rel.repeat(repeat_num, 1).to(self.device)
        attrs = attr.repeat(1, 1).to(self.device)
        numerical_prediction, pred_idx = self.score_model.get_numerical_prediction(head=[head_idx], attr=attrs,
                                                                                   bias=bias)

        relation_idx = [relation_idx] * len(heads)
        scores = self.score_model.triple_distance_transR(heads, tails, rels, relation_idx)
        _, indices_top_k = torch.topk(scores, k=len(scores), largest=self.largest)
        labels_top_k = [self.idx2entity[tails_idx[index].item()] for index in indices_top_k]
        return labels_top_k[0], numerical_prediction

    def test(self):
        tail_pred_accuracy = 0
        total_error = []
        triple_subset = self.file_loader.load_all_triples(full_dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                                          dataset_name=self.dataset_name)

        for line in triple_subset:
            s, p, o = [l.strip() for l in line.split("\t")]
            # using the true head and rel, try to find the predicted o
            pred_o, pred_n = self.test_triple_distance(test_entity=s, test_rel=p)
            pred_n = pred_n * 1000
            if o == pred_o:
                tail_pred_accuracy += 1
            if o in self.value_dictionary:
                true_n = (self.value_dictionary[o])
                print("-------------------------------------")
                print("true numerical value", true_n)
                print("predicted value", pred_n)
                print("-------------------------------------")
                error = min(abs((pred_n - true_n) / true_n), 1)
                total_error.append(error)

        print(" - tail_pred_accuracy", tail_pred_accuracy / len(triple_subset))
        print(" - mean error percentage", sum(total_error) / len(triple_subset))

    # def test_question_prediction(self, question):
    #     # self.score_model.get_relation_prediction()
    #     questions =


if __name__ == "__main__":
    my_embedding = EmbeddingTest()
    my_embedding.test()
