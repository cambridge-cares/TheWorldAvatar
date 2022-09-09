from pprint import pprint

import numpy as np
import torch
import transformers
from torch import nn, optim, FloatTensor, LongTensor
import os
import pandas as pd
from torch.nn.functional import normalize
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import BertModel, BertTokenizer, AdamW, BertForSequenceClassification

from Marie.Util.location import DATA_DIR


# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

# def tokenize_question(question_text):
#     tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
#     max_len = 24
#     return tokenizer(question_text, padding='max_length', max_length=max_len, truncation=True,
#                      return_tensors="pt")


def to_categorical(y, num_classes):
    """ 1-hot encodes a tensor """
    return np.eye(num_classes, dtype='float32')[y]


class Dataset(TorchDataset):
    def __init__(self, df):
        super(Dataset, self).__init__()
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.df = df
        self.max_len = 24
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in self.df.iloc[:, 0]]
        # self.y = to_categorical(self.df.iloc[:, 1], 21)
        self.y = self.df.iloc[:, 1].tolist()

    def classes(self):
        return self.y

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.tokenized_questions[idx], self.y[idx]


class StandAloneBERT(nn.Module):

    def __init__(self, device=torch.device("cuda")):
        super(StandAloneBERT, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.dropout = nn.Dropout(0.5)
        self.linear = nn.Linear(768, 21)  # keep this model ...
        self.relu = nn.ReLU()  # a good bert -> relation idx model is trained, how do we project it?

    def load_model(self):
        self.load_state_dict(torch.load(os.path.join(DATA_DIR, 'bert_model')))

    def loss(self, emb_1, emb_2):
        # set target to be -1, which measures the dissimilarity
        assert emb_1.shape[1] == emb_2.shape[1]
        target = torch.LongTensor([-1]).to(self.device)
        return self.criterion(emb_1, emb_2, target).to(self.device)

    def distance(self, emb_1, emb_2):
        assert emb_1.shape[1] == emb_2.shape[1]
        # calculate the L1 distance between emb_1 and emb_2
        distance = (normalize(emb_1) - normalize(emb_2)).norm(p=1, dim=1).to(self.device)
        # print(distance)
        return distance

    def forward(self, question):
        input_ids = torch.reshape(question['input_ids'], (-1, 12))
        attention_mask = torch.reshape(question['attention_mask'], (-1, 12))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1]

        dropout_output = self.dropout(pooled_output)
        linear_output = self.linear(dropout_output)
        final_layer = self.relu(linear_output)

        return final_layer

    def project(self, question):
        _, pooled_output_pos = self.bert(input_ids=question['input_ids'].squeeze(1).to(self.device),
                                         attention_mask=question['attention_mask'].to(self.device),
                                         return_dict=False)
        projected_rel = normalize(self.bert_reduce_stack(pooled_output_pos.to(self.device)).to(self.device))
        return projected_rel


if __name__ == '__main__':
    # ===========================
    batch_size = 64
    learning_rate = 1e-7
    step = 0
    # ===========================
    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")
    print(f'=========== USING {device} ===============')
    # ===========================

    df = pd.read_csv(os.path.join(DATA_DIR, 'question_set_rel'), sep=',', header=None)
    # df = df.sample(frac=0.2)
    df_train, df_test = np.split(df.sample(frac=1, random_state=42), [int(.8 * len(df))])
    dataset_test = Dataset(df_test)
    dataset_train = Dataset(df_train)
    test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
    train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
    model = StandAloneBERT(device=device)
    optimizer = AdamW(model.parameters(), lr=learning_rate)
    criterion = nn.CrossEntropyLoss()
    model.load_state_dict(torch.load(os.path.join(DATA_DIR, 'bert_model')))
    model = model.cuda()
    for epoch in range(250):
        total_loss_train = 0
        total_acc_train = 0
        model.train()
        for train_batch in tqdm(train_dataloader):
            true_y = torch.LongTensor(train_batch[1]).to(device)
            predicted_y = model(train_batch[0]).cuda()
            acc = (predicted_y.argmax(dim=1) == true_y).sum().item()
            total_acc_train += acc

            loss = criterion(predicted_y, true_y)
            loss.backward()
            optimizer.step()
            step += 1
            total_loss_train += loss.mean().item()
        print(f'\ntotal_loss_train: {total_loss_train}')
        print(f'\ntotal_acc_train: {total_acc_train / len(dataset_train)}')
        if epoch % 5 == 0:
            ratio_list = []
            model.eval()
            total_acc_val = 0
            for test_batch in tqdm(test_dataloader):
                true_y = torch.LongTensor(test_batch[1]).to(device)
                predicted_y = model(test_batch[0]).cuda()
                acc = (predicted_y.argmax(dim=1) == true_y).sum().item()
                total_acc_val += acc
                # correct += ratio

            print(f'\ntotal_acc_val: {total_acc_val / len(dataset_test)}')
            torch.save(model.state_dict(), os.path.join(DATA_DIR, 'bert_model'))
            print('bert model is saved')
