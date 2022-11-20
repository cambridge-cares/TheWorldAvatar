import sys

from torch import no_grad
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm
import os
import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader

sys.path.append("../../..")
from Marie.Util.Dataset.CrossGraph_Dataset import CrossGraphDataset
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel


class CrossGraphTrainer:
    def __init__(self, df, test_step=10, epoch_num=100, batch_size=4, learning_rate=1e-5, gamma=1,
                 dataset_path="CrossGraph", save_model=False,
                 load_model=False):
        self.df = df
        self.dataset_path = dataset_path

        self.epoch_num = epoch_num
        self.test_step = test_step
        self.learning_rate = learning_rate
        self.gamma = gamma

        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        self.dataset_dir = os.path.join(DATA_DIR, "ontocompchem_latent_40")
        print(f'=========== USING {self.device} ===============')
        self.model = CrossGraphAlignmentModel(device=self.device).to(self.device)
        self.optimizer = torch.optim.Adam(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

        # df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])
        df_train = df
        df_test = df.sample(frac=0.2)
        dataset_train = CrossGraphDataset(df_train)
        self.dataloader_train = DataLoader(dataset_train, batch_size=batch_size, shuffle=False)
        dataset_test = CrossGraphDataset(df_test)
        self.dataloader_test = DataLoader(dataset_test, batch_size=batch_size, shuffle=False)
        # ============= init the model ===============

    def run(self):
        previous_rate = 0
        stop = False
        for epoch in tqdm(range(self.epoch_num)):
            stop = self.train()
            if epoch % self.test_step == 0:
                self.evaluate()
                self.save_model()
            if stop:
                self.evaluate()
                self.save_model()
                break


    def train(self):
        total_loss_train = 0
        total_outrank_rate = 0
        for true_answer, fake_answer, true_domain, fake_domain, question in tqdm(self.dataloader_train):
            pos_triple = (question, true_answer, true_domain)  # question, score, domain
            loss, pred_domain = self.model(true_answer=pos_triple)
            pred_domain = pred_domain > 0.5
            total_outrank_rate += torch.sum(pred_domain == true_domain) / len(pred_domain)
            loss.mean().backward()
            total_loss_train += loss.mean().cpu()

        self.optimizer.step()
        if total_loss_train < 1 and total_outrank_rate / len(self.dataloader_train) >= 1.0:
            return True
        print(f'current learning rate {self.scheduler.get_last_lr()}')
        print(f"total train loss {total_loss_train}")
        print(f"total out rank rate {total_outrank_rate / len(self.dataloader_train)}")
        return False

    def evaluate(self):
        """
        Evaluation matrix : [outrank rate, which measures the percentage of fake answers returned from
        another embedding space... ]
        :return:
        """
        total_test_outrank_rate = 0
        for true_answer, fake_answer, true_domain, fake_domain, question in tqdm(self.dataloader_test):
            with no_grad():
                pos_triple = (question, true_answer, true_domain)  # question, score, domain
                neg_triple = (question, fake_answer, fake_domain)
                true_scores = self.model.predict(pos_triple)
                fake_scores = self.model.predict(neg_triple)
                outrank = true_scores > fake_scores
                outrank_rate = torch.sum(outrank) / len(true_scores)
                total_test_outrank_rate += outrank_rate

        total_test_outrank_rate = total_test_outrank_rate / len(self.dataloader_test)
        print(f"total test outrank rate {total_test_outrank_rate}")

    def save_model(self):
        model_path = os.path.join(self.dataset_path, 'cross_graph_model')
        print(' - Saving the scoring model')
        torch.save(self.model.state_dict(), model_path)


if __name__ == '__main__':
    dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
    df = pd.read_csv(os.path.join(dataset_path, 'cross_graph_pairs.tsv'), sep='\t', index_col=0)
    my_cross_graph_trainer = CrossGraphTrainer(df, dataset_path=dataset_path)
    my_cross_graph_trainer.run()
