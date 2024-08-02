import os

import numpy as np
import pandas as pd
import torch
from torch import nn
from torch.optim import Adam
from tqdm import tqdm
from transformers import BertModel, BertTokenizer
from Training.ScoringFunction.playground.location import PLAYGROUND_DIR
from Marie.Util.Embedding.Embedding import Embedding

tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
labels = {0: 0, 1: 1}
from pytorch_pretrained_bert.optimization import BertAdam, WarmupLinearSchedule
import torch.nn.functional as F


# labels = {'business':0,
#           'entertainment':1,
#           'sport':2,
#           'tech':3,
#           'politics':4
#           }
# integrate the TransE model as one of the scoring .... basically transform the question into the relation and do
# the scoring ...

class TorchBERTModel(nn.Module):

    def kge_loss(self, scores, targets):
        # loss = torch.mean(scores*targets)
        return self._klloss(
            F.log_softmax(scores, dim=1), F.normalize(targets.float(), p=1, dim=1)
        )

    def __init__(self, dropout=0):
        super(TorchBERTModel, self).__init__()

        self.embed_util = Embedding()

        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.dropout = nn.Dropout(dropout)
        self.relu = nn.ReLU()

        self.logsoftmax = torch.nn.LogSoftmax(dim=-1)
        self._klloss = torch.nn.KLDivLoss(reduction='sum')

        self.sigmoid = nn.Sigmoid()

        # =========================
        # build layer stack for reducing bert result 768 -> 64
        self.bert_reduce_stack = nn.Sequential(
            nn.Linear(768, 512),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(512, 256),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(256, 128),
            nn.ReLU(),
            nn.Dropout(dropout),
            nn.Linear(128, 113)
        )
        # =========================
        # build layer stack for reducing kg embedding 113 -> 64
        self.kg_reduce_stack = nn.Sequential(
            nn.Linear(113, 64),
            nn.ReLU(),
            nn.Dropout(dropout)
        )

        # =========================
        # build stack for actual scoring function 994 -> 2
        self.score_stack = nn.Sequential(
            nn.Linear(994, 512),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(512, 256),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(256, 256),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(256, 256),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(256, 64),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(64, 32),
            nn.ReLU(),
            nn.Dropout(dropout),

            nn.Linear(32, 2),
            nn.ReLU(),
            nn.Sigmoid(),

        )

        self.pdist = nn.PairwiseDistance(p=2)

    def forward(self, input_id, mask, head_entity, tail_entity):
        _, pooled_output = self.bert(input_ids=input_id, attention_mask=mask, return_dict=False)
        nlp_output = self.bert_reduce_stack(pooled_output)
        score = self.pdist(head_entity + nlp_output, tail_entity)
        output = self.relu(score)
        return output


class Dataset(torch.utils.data.Dataset):

    def __init__(self, df):
        self.embedding_util = Embedding()
        self.labels = [labels[label] for label in df['score']]

        self.texts = [tokenizer(text,
                                padding='max_length', max_length=12, truncation=True,
                                return_tensors="pt") for text in df['question']]
        self.head_entity = [head for head in df['head']]
        self.tail_entity = [tail for tail in df['tail']]

    def classes(self):
        return self.labels

    def __len__(self):
        return len(self.labels)

    def get_batch_labels(self, idx):
        # Fetch a batch of labels
        rst = np.array(self.labels[idx])
        return rst

    def get_batch_texts(self, idx):
        # Fetch a batch of inputs
        rst = self.texts[idx]
        return rst

    def get_embedding(self, _name):
        return self.embedding_util.ent2embedding(_name)

    def get_batch_e_h(self, idx):
        return self.get_embedding(self.head_entity[idx])

    def get_batch_e_t(self, idx):
        return self.get_embedding(self.tail_entity[idx])

    def __getitem__(self, idx):
        batch_texts = self.get_batch_texts(idx)
        batch_y = self.get_batch_labels(idx)
        batch_e_h = torch.FloatTensor(self.get_batch_e_h(idx))
        batch_e_t = torch.FloatTensor(self.get_batch_e_t(idx))
        kg_embeddings = {'e_h': batch_e_h, 'e_t': batch_e_t}

        return batch_texts, batch_y, kg_embeddings


def train(model, train_data, val_data, learning_rate=1e-6, epochs=50, batch_size=32):
    torch.cuda.empty_cache()
    train, val = Dataset(train_data), Dataset(val_data)

    train_dataloader = torch.utils.data.DataLoader(train, batch_size=batch_size, shuffle=True)
    val_dataloader = torch.utils.data.DataLoader(val, batch_size=batch_size, shuffle=True)

    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda" if use_cuda else "cpu")

    print(f'=========== USING {device} ===============')

    criterion = nn.CrossEntropyLoss()
    optimizer = BertAdam(model.parameters(), lr=learning_rate)

    if use_cuda:
        model = model.cuda()
        criterion = criterion.cuda()

    for epoch_num in range(epochs):

        total_acc_train = 0
        total_loss_train = 0

        for train_input, train_label, train_kg_embedding in tqdm(train_dataloader):
            train_label = train_label.to(device)
            mask = train_input['attention_mask'].to(device)
            input_id = train_input['input_ids'].squeeze(1).to(device)

            head_embedding = train_kg_embedding['e_h'].to(device)
            tail_embedding = train_kg_embedding['e_t'].to(device)

            output = model(input_id, mask, head_embedding, tail_embedding)

            batch_loss = criterion(output, train_label.long())
            total_loss_train += batch_loss.item()

            acc = (output.argmax(dim=1) == train_label).sum().item()
            total_acc_train += acc

            model.zero_grad()
            batch_loss.backward()
            optimizer.step()

        total_acc_val = 0
        total_loss_val = 0

        with torch.no_grad():

            for val_input, val_label, val_kg_embedding in val_dataloader:
                val_label = val_label.to(device)
                mask = val_input['attention_mask'].to(device)
                input_id = val_input['input_ids'].squeeze(1).to(device)
                head_embedding = val_kg_embedding['e_h'].to(device)
                tail_embedding = val_kg_embedding['e_t'].to(device)

                output = model(input_id, mask, head_embedding, tail_embedding)

                batch_loss = criterion(output, val_label.long())
                total_loss_val += batch_loss.item()

                acc = (output.argmax(dim=1) == val_label).sum().item()
                total_acc_val += acc

        print(
            f'Epochs: {epoch_num + 1} \n| Train Loss: {total_loss_train / len(train_data) * 100000: .8f} \
                \n| Train Accuracy: {total_acc_train / len(train_data): .4f} \
                \n| Val Loss: {total_loss_val / len(val_data) * 100000: .8f} \
                \n| Val Accuracy: {total_acc_val / len(val_data): .4f}')


if __name__ == '__main__':
    EPOCHS = 100
    model = TorchBERTModel()
    LR = 5e-10
    np.random.seed(112)
    datapath = os.path.join(PLAYGROUND_DIR, 'question_set_full')
    df = pd.read_csv(datapath, sep='\t')
    # df = df.sample(frac=0.3)
    df.head()
    df_train, df_val, df_test = np.split(df.sample(frac=1, random_state=42), [int(.8 * len(df)), int(.9 * len(df))])
    train(model, df_train, df_val, LR, EPOCHS, batch_size=128)
