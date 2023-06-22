import json
import sys

import torchmetrics
from torch import no_grad
from torch.optim.lr_scheduler import ExponentialLR
from torchmetrics import Accuracy
from tqdm import tqdm
import os
import numpy as np
import pandas as pd
import torch
from torch.utils.data import DataLoader
sys.path.append("")
sys.path.append("../../Marie/Util")
sys.path.append("../../Marie")
sys.path.append("../..")
sys.path.append("../../..")
sys.path.append("../../../..")
from Marie.Util.Dataset.CrossGraph_Dataset import CrossGraphDataset
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.CrossGraphAlignmentModel import CrossGraphAlignmentModel


class CrossGraphTrainer:
    def __init__(self, df, test_step=50, epoch_num=1000, batch_size=32, learning_rate=1e-5, gamma=1,
                 dataset_path="CrossGraph", save_model=False,
                 load_model=False):
        self.df = df
        self.dataset_path = dataset_path
        self.load_model = load_model
        self.epoch_num = epoch_num
        self.test_step = test_step
        self.learning_rate = learning_rate
        self.gamma = gamma

        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {self.device} ===============')
        self.model = CrossGraphAlignmentModel(device=self.device).to(self.device)
        if self.load_model:
            self.model.load_state_dict(torch.load(os.path.join(dataset_path, "cross_graph_model_with_all_9_updated")))
        self.optimizer = torch.optim.Adam(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)
        # print(df["true_domain"])
        # df["true_domain"].apply(eval)
        # df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])
        df_train = df
        df_test = df.sample(frac=1)
        dataset_train = CrossGraphDataset(df_train)
        self.dataloader_train = DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        dataset_test = CrossGraphDataset(df_test)
        self.dataloader_test = DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        # ============= init the model ===============

    def run(self):
        previous_rate = 0
        stop = False
        for epoch in tqdm(range(self.epoch_num)):
            print(f"============ Epoch {epoch} ============")
            stop = self.train()
            if (epoch + 1) % self.test_step == 0:
                # self.evaluate()
                self.scheduler.step()
                self.save_model()
            if stop:
                # self.evaluate()
                self.save_model()
                break
        # self.evaluate()

    def train(self):
        total_loss_train = 0
        total_outrank_rate = 0
        for true_score, true_domain, question in tqdm(self.dataloader_train):
            true_domain = [json.loads(d) for d in true_domain]
            # new_true_domain = []
            # for domain_list in true_domain:
            #     counter = 0
            #     row = []
            #     for d in domain_list:
            #         if d == 1:
            #             # put the label there
            #             row.append(counter)
            #         else:
            #             row.append(-1)
            #         counter += 1
            #     new_true_domain.append(sorted(row, reverse=True))
            # true_domain = new_true_domain
            # print(true_domain)
            pos_triple = (question, true_score, true_domain)  # question, score, domain
            loss, pred_domain = self.model(true_answer=pos_triple)
            pred_domain = pred_domain.cpu()
            # pred_domain = pred_domain.type(torch.LongTensor)
            true_domain = torch.LongTensor(true_domain)
            # print(pred_domain)
            accuracy = Accuracy(task="multilabel", num_labels=9)
            hit_rate = accuracy(pred_domain, true_domain)

            # hit_rate = torchmetrics.functional.accuracy(task="multilabel",pred_domain, true_domain)
            total_outrank_rate += hit_rate
            loss.mean().backward()
            total_loss_train += loss.sum().cpu()
        print(f"total_outrank_rate: {total_outrank_rate / len(self.dataloader_train)}")

        self.optimizer.step()
        if total_outrank_rate / len(self.dataloader_train) >= 1.0:
            return True
        # print(f'current learning rate {self.scheduler.get_last_lr()}')
        print(f"total train loss {total_loss_train}")
        # print(f"total out rank rate {total_outrank_rate / len(self.dataloader_train)}")
        return False

    def evaluate(self):
        """
        Evaluation matrix : [outrank rate, which measures the percentage of fake answers returned from
        another embedding space... ]
        :return:
        """
        total_test_outrank_rate = 0
        counter = 0
        for true_score, true_domain, question in tqdm(self.dataloader_test):
            with no_grad():
                pred_domain = torch.LongTensor(self.model.predict(question))
                true_domain = torch.LongTensor([json.loads(d) for d in true_domain])

                print(pred_domain)
                print(true_domain)

                factor = torch.eq(pred_domain, true_domain).type(torch.LongTensor)
                factor = torch.sum(factor, dim=1) == 5
                factor = factor.type(torch.LongTensor)
                factor = torch.sum(factor, dim=0)
                counter += len(factor)

                # pos_triple = (question, true_answer, true_domain)  # question, score, domain
                # neg_triple = (question, fake_answer, fake_domain)
                # true_scores = self.model.predict(pos_triple).to(self.device)
                # fake_scores = self.model.predict(neg_triple).to(self.device)
                # outrank = true_scores > fake_scores
                # outrank_rate = torch.sum(outrank) / len(true_scores)
                total_test_outrank_rate += factor

        total_test_outrank_rate = total_test_outrank_rate / len(self.dataloader_test)
        print(f"total test outrank rate {total_test_outrank_rate}")

    def save_model(self):
        model_path = os.path.join(self.dataset_path, 'cross_graph_model_with_all_9_updated')
        print(' - Saving the scoring model')
        torch.save(self.model.state_dict(), model_path)


if __name__ == '__main__':
    dataset_path = os.path.join(DATA_DIR, 'CrossGraph')
    df = pd.read_csv(os.path.join(dataset_path, 'cross_graph_alignment_training_updated.tsv'), sep='\t',
                     index_col=None)
    # df.columns = ["question", "true_score", "true_domain"]
    my_cross_graph_trainer = CrossGraphTrainer(df, dataset_path=dataset_path, load_model=False)
    my_cross_graph_trainer.run()
