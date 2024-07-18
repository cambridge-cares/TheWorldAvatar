from ast import literal_eval

import pandas as pd
import os

import torch

from Marie.PubChem import PubChemEngine
from Marie.OntoCompChem import OntoCompChemEngine
from Marie.Util.location import DATA_DIR
from torch.nn.functional import one_hot


class CrossGraphDatasetCreator:

    def __init__(self):
        self.dataset_path = os.path.join(DATA_DIR, 'CrossGraph')

    def run(self):
        df = pd.read_csv(os.path.join(self.dataset_path, 'cross_graph_pairs.tsv'), sep='\t', index_col=0)
        df = df.loc[df["fake_score"] >= 1]
        df = df.loc[df["true_score"] >= 1]
        df = df.apply(lambda x: x.str.strip() if x.dtype == "object" else x)
        df = df.drop(columns=["head", "fake_score", "fake_domain"])
        df = df.drop_duplicates()
        df_comp_and_pub = df.reset_index(drop=True)

        df_ontokin = pd.read_csv(os.path.join(self.dataset_path, 'ontokin', 'score_model_training.tsv'),
                                 sep='\t', index_col=0)
        df_ontokin = df_ontokin.drop(columns=["head", "tail", "rel"])
        df_ontokin.insert(1, "true_score", 1.0)
        df_ontokin.insert(2, "true_domain", 2)
        df_ontokin = df_ontokin.drop_duplicates()
        df_ontospecies = pd.read_csv(os.path.join(self.dataset_path, 'ontospecies', 'score_model_training.tsv'),
                                     sep='\t', index_col=0)
        df_ontospecies = df_ontospecies.drop(columns=["head", "tail", "rel"])
        df_ontospecies.insert(1, "true_score", 1.0)
        df_ontospecies.insert(2, "true_domain", 3)
        df_ontospecies = df_ontospecies.drop_duplicates()

        df = pd.concat([df_comp_and_pub, df_ontokin, df_ontospecies])
        # df.to_csv("test.tsv", sep="\t", index=False)
        df = pd.read_csv("test.tsv", sep='\t')
        # df.true_domain = df.true_domain.apply(literal_eval)
        df = df.drop_duplicates()

        rows = []
        for idx,row in df.iterrows():
            print(row)
            question, true_score, domains = row
            if ',' in domains:
                domains = [int(d) for d in domains.split(',')]
            else:
                domains = [int(domains)]

            print("-----------")
            print(domains)
            domains = torch.LongTensor(domains)
            domains = one_hot(domains, num_classes=4)
            domains = torch.transpose(domains, 1, 0)
            domains = torch.sum(domains, dim=1).tolist()
            rows.append((question, true_score, domains))

        df = pd.DataFrame(rows, columns=["question", "true_score", "true_domain"])
        df.to_csv("xxx.tsv", sep="\t")

if __name__ == "__main__":
    my_creator = CrossGraphDatasetCreator()
    my_creator.run()
