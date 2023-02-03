import sys

from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

sys.path.append("../../..")

import json
import os

import pandas as pd
import torch

from Marie.Util.Models.TransR import TransR
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.Dataset.TransR_Inference_Dataset import TransRInferenceDataset
from Marie.Util.location import DATA_DIR


class InferenceTrainer:

    def __init__(self, full_dataset_dir, ontology, batch_size=32, epoch_num=20):
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.learning_rate = 0.01
        self.gamma = 0.1
        df_train = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-train-2.txt"), sep="\t", header=None)
        df_test = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-test.txt"), sep="\t", header=None)



        test_set = TransRInferenceDataset(df_test, full_dataset_dir=self.full_dataset_dir, ontology=self.ontology,
                                          mode="test")

        train_set = TransRInferenceDataset(df_train, full_dataset_dir=self.full_dataset_dir, ontology=self.ontology,
                                           mode="train")

        self.test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=1, shuffle=True)
        self.train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size, shuffle=True)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()

        # ------------------------- Training hyperparameters -----------------------
        self.epoch_num = epoch_num
        self.model = TransR(rel_dim=20, rel_num=len(self.rel2idx.keys()), ent_dim=20, ent_num=len(self.entity2idx.keys()))
        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

    def evaluate(self):
        for test_set in tqdm(self.test_dataloader):
            distances, true_idx = self.model.predict(test_set)
            _, B = torch.topk(distances, k=10)
            print(B)
            print(true_idx)
            # self.model.infer_reaction(test_set[0], rel2idx=self.rel2idx)

    def train(self):
        total_train_loss = 0
        for pos_triples, neg_triples in tqdm(self.train_dataloader):
            loss = self.model(pos_triples, neg_triples)
            loss.backward()
            self.optimizer.step()
            total_train_loss += loss.cpu().mean()
        print(f"Loss: {total_train_loss}")



    def reaction_inference(self):
        pass

    def run(self):
        for epoch in range(self.epoch_num + 1):
            self.train()
            if epoch % 10 == 0:
                self.scheduler.step()
                self.evaluate()



if __name__ == "__main__":
    full_dir = os.path.join(DATA_DIR, 'CrossGraph', 'ontospecies_new/role_only')
    ontology = "role_only"
    my_trainer = InferenceTrainer(full_dataset_dir=full_dir, ontology=ontology, batch_size=128)
    my_trainer.run()
