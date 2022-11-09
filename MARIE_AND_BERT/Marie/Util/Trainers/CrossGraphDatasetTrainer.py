from tqdm import tqdm
import os
import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader

from Marie.Util.Dataset.CrossGraph_Dataset import CrossGraphDataset
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel


class CrossGraphTrainer:
    def __init__(self, df, test_step=50, epoch_num=100, batch_size=32, learning_rate=0.1, gamma=1, save_model=False,
                 load_model=False):
        self.df = df
        self.epoch_num = epoch_num
        self.test_step = test_step
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        self.dataset_dir = os.path.join(DATA_DIR, "ontocompchem_latent_40")
        print(f'=========== USING {self.device} ===============')
        self.model = CrossGraphAlignmentModel(device=self.device).to(self.device)

        # ============= init the model ===============

    def run(self):
        for epoch in tqdm(range(self.epoch_num)):
            self.train()

            if epoch % self.test_step == 0:
                self.evaluate()

    def train(self):
        df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])

        dataset_test = CrossGraphDataset(df_test)
        dataloader_test = DataLoader(dataset_test, batch_size=32, shuffle=True)
        for true_answer, fake_answer, true_domain, fake_domain, question in dataloader_test:
            true_answer = torch.cat(true_answer)
            fake_answer = torch.cat(fake_answer)
            true_domain = torch.cat(true_domain)
            fake_domain = torch.cat(fake_domain)
            # attention_mask = question['attention_mask']
            # input_ids = question['input_ids']
            # print(attention_mask)
            # print(input_ids)
            #
            # # true_answer = true_answer[0]
            # # fake_answer = fake_answer[0]
            # # true_domain = true_domain[0]
            # # fake_domain = fake_domain[0]

            pos_triple = (question, true_answer, true_domain)  # question, score, domain
            neg_triple = (question, fake_answer, fake_domain)
            self.model(true_answer=pos_triple, fake_answer=neg_triple).to(self.device)

    def evaluate(self):
        pass


if __name__ == '__main__':
    df = pd.read_csv(os.path.join(DATA_DIR, 'CrossGraph', 'cross_graph_pairs.tsv'), sep='\t')
    my_cross_graph_trainer = CrossGraphTrainer(df)
    my_cross_graph_trainer.train()
