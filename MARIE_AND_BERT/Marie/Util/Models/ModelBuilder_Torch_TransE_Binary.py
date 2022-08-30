import os
import sys

import numpy as np
import pandas as pd
from tqdm import tqdm
from transformers import BertModel, BertConfig

sys.path.append('../../../')

from torch import nn, optim
import torch

from Marie.Util.Models.Binary_Dataset import Dataset
from Marie.Util.location import DATA_DIR


class ScoreModelWithLabel(nn.Module):

    def __init__(self, ent_embedding, device, dropout=0.0):
        super(ScoreModelWithLabel, self).__init__()

        self.bert_reduce_stack = nn.Sequential(
            nn.Linear(768, 50),
            nn.ReLU(),
            nn.Dropout(dropout),

            # nn.Linear(512, 256),
            # nn.ReLU(),
            # nn.Dropout(dropout),
            #
            # nn.Linear(256, 128),
            # nn.ReLU(),
            # nn.Dropout(dropout),

            # nn.Linear(512, 50),
            # nn.ReLU(),
            # nn.Dropout(dropout)

        )

        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.ent_embedding = ent_embedding
        self.device = device
        self.linear = nn.Linear(50, 50)
        self.softmax = nn.Softmax(dim = 0)

        # self.linear_1 = nn.Sequential(
        #     nn.Linear(1, 1),
        #     nn.ReLU()
        # )

        self.linear_1 = nn.Linear(1, 1)
        self.linear_2 = nn.Linear(1, 1)
        self.linear_3 = nn.Linear(1, 1)
        self.linear_50_1 = nn.Linear(50, 1)

        self.sigmoid = nn.Sigmoid()

    def forward(self, triplets):
        nlp_components = triplets['question']
        _, pooled_output = self.bert(input_ids=nlp_components["input_ids"].squeeze(1).to(self.device),
                                     attention_mask=nlp_components["attention_mask"].to(self.device),
                                     return_dict=False)

        projected_rel = self.bert_reduce_stack(pooled_output)
        distance = self.distance(triplets, projected_rel)
        distance = self.linear_50_1(distance)
        # distance = torch.unsqueeze(distance, 1).type(torch.float32)
        # distance = self.linear_1(distance)
        # distance = self.linear_2(distance)
        # distance = self.linear_3(distance)

        result = self.softmax(distance)
        return result

    def distance(self, triplets, projected_rel):
        e_h_idx = triplets['e_h']
        e_t_idx = triplets['e_t']
        head = torch.tensor(self.ent_embedding.iloc[e_h_idx].values).to(self.device)
        # head = self.linear(head.type(torch.FloatTensor).to(self.device)).to(self.device)
        tail = torch.tensor(self.ent_embedding.iloc[e_t_idx].values).to(self.device)
        # tail = self.linear(tail.type(torch.FloatTensor).to(self.device)).to(self.device)
        # tail = self.linear_1(tail)

        # return (head + projected_rel - tail).norm(p=1, dim=1).to(self.device)
        return (head + projected_rel - tail).type(torch.FloatTensor).to(self.device)


class Trainer:

    def __init__(self, batch_size):
        # =============== get data ready ============
        df_path = os.path.join(DATA_DIR, 'question_set_full')
        df = pd.read_csv(df_path, sep='\t')
        df_train, df_test = np.split(df.sample(frac=1, random_state=23), [int(.8 * len(df))])
        self.train_set = Dataset(df_train)
        self.test_set = Dataset(df_test)
        self.train_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=batch_size, shuffle=True)
        # =============== init parameters =============
        self.learning_rate = 5
        self.loss = nn.BCELoss()
        self.epoches = 5000
        self.step = 0

        # =============== init model ==================
        self.ent_embedding = self.train_set.ent_embedding
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        self.model = ScoreModelWithLabel(self.ent_embedding, self.device, dropout=0.0).to(self.device)
        self.optimizer = optim.SGD(self.model.parameters(), lr=self.learning_rate)

    def train(self):
        with tqdm(total=self.epoches, unit=' epoch') as tepoch:
            for epoch_num in range(self.epoches):
                one_count = 0
                temp = None
                truth = None
                tepoch.set_description(f'Epoch {epoch_num}')
                self.model.train()
                total_loss_train = 0
                for train_x, train_y in tqdm(self.train_dataloader):
                    self.optimizer.zero_grad()

                    # int_pred_y = pred_y.type(torch.IntTensor)
                    # ones = (pred_y == 1.).sum(dim=0).sum().item()
                    # one_count += ones
                    pred_y = self.model(train_x)

                    loss = self.loss(torch.squeeze(pred_y, 1).type(torch.DoubleTensor),
                                     train_y.type(torch.DoubleTensor))
                    temp = pred_y
                    truth = train_y
                    loss.mean().backward()
                    loss = loss.data.cuda()
                    self.optimizer.step()
                    self.step += 1
                    total_loss_train += loss.mean().item()
                print(f'total_loss_train: {total_loss_train}')
                print(f'total one count {one_count}')
                print(temp.squeeze(1))
                print(truth)


if __name__ == '__main__':
    trainer = Trainer(batch_size=16)
    trainer.train()
    # x  = torch.tensor([0,1])
    # y = torch.unsqueeze(x, 0)
    # print(y)
    # z = torch.unsqueeze(x, 1)
    # print(z)
