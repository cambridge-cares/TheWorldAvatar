import json
import sys
from torch.optim.lr_scheduler import ExponentialLR
import numpy as np
import torch
from torch import nn, no_grad
import os
import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import BertModel, BertTokenizer, AdamW

sys.path.append("../../..")
from Marie.Util.location import TRAINING_DIR, DEPLOYMENT_DIR, DATA_DIR
from KGToolbox.NHopExtractor import HopExtractor

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


class Dataset(TorchDataset):
    def __init__(self, df, embedding_path):
        """
        This dataset provides (question, head, tail, score (binary))
        :param df:
        """
        super(Dataset, self).__init__()
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')
        self.df = df
        self.max_len = 12
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in self.df['question']]

        embedding_path = os.path.join(DATA_DIR, 'ontocompchem_calculation')
        self.ent_embedding = pd.read_csv(os.path.join(embedding_path, 'ent_embedding.tsv'), sep='\t', header=None)
        self.rel_embedding = pd.read_csv(os.path.join(embedding_path, 'rel_embedding.tsv'), sep='\t', header=None)
        # self.heads = self.ent_embedding.iloc[self.df["head"].tolist()]
        # self.tails = self.ent_embedding.iloc[self.df["tail"].tolist()]
        self.heads = self.df["head"].tolist()
        self.tails = self.df["tail"].tolist()

        self.y = self.df["score"]

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        return self.tokenized_questions[idx], self.heads[idx], \
               self.tails[idx], self.y.iloc[idx]


class StandAloneBERT(nn.Module):
    """
    The upgraded version of the prediction mapping model, which able to derive the implicit relation on the fly.
    Input: question, head, tail
    Output: e_q
    Training: score(head, predicted_rel, tail), compare the score to the real score ... true / fake
    """

    def __init__(self, ent_embedding=None, rel_embedding=None, for_training=False, device=torch.device("cpu")):
        super(StandAloneBERT, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-cased')
        self.dropout = nn.Dropout(0)
        self.linear = nn.Linear(768, 80)  # keep this model ...
        # self.linear_1 = nn.Linear(80, 40)
        self.sigmoid = nn.Sigmoid()

        self.criterion = torch.nn.CosineEmbeddingLoss()
        self.for_training = for_training
        if self.for_training:
            self.model_dir = TRAINING_DIR
        else:
            self.model_dir = DEPLOYMENT_DIR

        embedding_dim = int(ent_embedding.shape[1])
        half = int(embedding_dim / 2)
        self.re_ent = ent_embedding.iloc[:, :half]  # first half of the embedding
        self.im_ent = ent_embedding.iloc[:, half:]
        self.re_rel = rel_embedding.iloc[:, :half]
        self.im_rel = rel_embedding.iloc[:, half:]
        self.half = half
        self.criterion = nn.BCELoss()

        # TODO: load the embeddings and divide them into two parts re and im

    def load_embedding(self, embedding_path):
        pass

    def pointwise_bce(self, preds, target):
        """
        :param preds: model predicted score of the triple
        :param target: true score of the triple, which is a binary value 0 or 1 ...
        :return: Binary Cross Enthropy with Logits loss (Built in sigmoid activation)
        """

        loss = torch.nn.BCEWithLogitsLoss()(preds, target)
        return loss

    def score(self, triple):
        head_idx = triple[0]
        predicted_rel = triple[1]
        tail_idx = triple[2]
        re_head = torch.FloatTensor(self.re_ent.iloc[head_idx].values.tolist()).to(self.device)
        im_head = torch.FloatTensor(self.im_ent.iloc[head_idx].values.tolist()).to(self.device)

        re_rel = predicted_rel[:, :self.half]
        im_rel = predicted_rel[:, self.half:]

        re_tail = torch.FloatTensor(self.re_ent.iloc[tail_idx].values.tolist()).to(self.device)
        im_tail = torch.FloatTensor(self.im_ent.iloc[tail_idx].values.tolist()).to(self.device)

        pred = - torch.sum(re_head * re_tail * re_rel + im_head * im_tail * re_rel +
                           re_head * im_tail * im_rel - im_head * re_tail * im_rel, -1).to(self.device)

        return pred

    def forward(self, question, head, tail, y):
        """
        :param question: tokenized question ({"attention_mask": [0,0,...,1]})
        :param head: idx of the head entity
        :param tail: idx of the tail entity
        :param y: the score of the triple, either 0 or 1
        :return:
        """
        input_ids = torch.reshape(question['input_ids'], (-1, max_len))
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len))
        pooled_output = self.bert(input_ids=input_ids.to(self.device),
                                  attention_mask=attention_mask.to(self.device),
                                  return_dict=False)[1].to(self.device)

        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        triple_score = self.score((head, predicted_rel, tail)).to(self.device)
        # triple_score = self.sigmoid(triple_score)
        return self.pointwise_bce(triple_score.type(torch.FloatTensor).to(self.device),
                                  y.type(torch.FloatTensor).to(self.device)).to(self.device), triple_score

    def predict(self, question, head, tail):
        input_ids = torch.reshape(question['input_ids'], (-1, max_len)).to(self.device)
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len)).to(self.device)
        pooled_output = self.bert(input_ids=input_ids,
                                  attention_mask=attention_mask,
                                  return_dict=False)[1].to(self.device)

        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        triple_score = self.score((head, predicted_rel, tail)).to(self.device)
        # triple_score = self.sigmoid(triple_score)
        return triple_score


class Trainer():

    def __init__(self, batch_size=64, epoch_num=100, learning_rate=0.1, gamma = 1):
        self.batch_size = batch_size
        self.epoch_num = epoch_num
        self.learning_rate = learning_rate
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {self.device} ===============')

        dataset_path = os.path.join(DATA_DIR, 'ontocompchem_calculation', 'score_model_training.tsv')
        df = pd.read_csv(dataset_path, sep="\t")
        df = df.sample(frac=1)
        df_train, df_test = np.split(df.sample(frac=1, random_state=41), [int(.8 * len(df))])
        dataset_full = Dataset(df, 'ontocompchem_calculation')
        dataset_test = Dataset(df_test, 'ontocompchem_calculation')
        dataset_train = Dataset(df_train, 'ontocompchem_calculation')
        self.test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        self.train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        self.ent_embedding = dataset_full.ent_embedding
        self.rel_embedding = dataset_full.rel_embedding
        self.ent_embedding_num = self.ent_embedding.shape[0]

        self.model = StandAloneBERT(device=self.device, ent_embedding=self.ent_embedding,
                                    rel_embedding=self.rel_embedding, for_training=True).cuda()
        self.optimizer = AdamW(self.model.parameters(), lr=self.learning_rate)
        self.model_name = "score_model_general"
        self.model_path = os.path.join(DATA_DIR, "ontocompchem_calculation", self.model_name)
        self.load_model()
        self.gamma = gamma
        self.scheduler = ExponentialLR(self.optimizer, gamma = self.gamma)
        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, "ontocompchem_calculation"),
                                          dataset_name="ontocompchem_calculation")

    def load_model(self):

        if os.path.exists(self.model_path):
            print(" - Loading pretrained BERT Mapping model")
            self.model.load_state_dict(torch.load(self.model_path))

        else:
            print(" - Model doesn't exist ")

    def save_model(self):
        print(' - Saving the scoring model')
        torch.save(self.model.state_dict(), self.model_path)

    def measure_hit_binary(self, predicted_y, true_y):
        # print("predicted_y", predicted_y)
        # TODO: do it the numpy
        predicted_y = predicted_y.cpu().detach().numpy()
        predicted_y[predicted_y > 0] = 1
        predicted_y[predicted_y <= 0] = 0
        true_y = true_y.cpu().detach().numpy()
        count = np.sum(predicted_y == true_y)
        rate = count / len(true_y)
        return rate

    def extract_n_hop_subgraph(self, head, hop=3):
        pass

    def hit_at_k(self, predictions, ground_truth_idx=0, k: int = 10):
        k = min(len(predictions), k)
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=True)
        if ground_truth_idx in indices_top_k.to(self.device):
            return 1
        else:
            return 0

    def rank_candidates(self, batch):
        question, head, tail, score = batch
        input_ids = question['input_ids']
        attention_mask = question['attention_mask']
        hit_1_count = 0
        hit_5_count = 0
        hit_10_count = 0
        for i_i, a_m, h, t, s in zip(input_ids, attention_mask, head, tail, score):
            with no_grad():
                if s == 1:
                    # repeat the tensor n many times
                    all_tails = self.hop_extractor.extract_neighbour_from_idx(h.item())
                    all_tails = torch.LongTensor(all_tails)
                    t = torch.cat([t.unsqueeze(0), all_tails])
                    i_i_batch = i_i.repeat(len(t), 1)
                    a_m_batch = a_m.repeat(len(t), 1)
                    h = h.repeat(len(t))
                    q = {"input_ids": i_i_batch, "attention_mask": a_m_batch}
                    prediction = self.model.predict(q, h, t)
                    hit_1_count += self.hit_at_k(prediction, k=1)
                    hit_5_count += self.hit_at_k(prediction, k=5)
                    hit_10_count += self.hit_at_k(prediction, k=10)

        hit_1_rate = hit_1_count / len(score)
        hit_5_rate = hit_5_count / len(score)
        hit_10_rate = hit_10_count / len(score)
        return hit_1_rate, hit_5_rate, hit_10_rate

    def evaluate(self):
        self.model.eval()

        total_hit_1_rate = 0
        total_hit_5_rate = 0
        total_hit_10_rate = 0
        total_test_accuracy = 0
        with no_grad():
            for test_batch in tqdm(self.test_dataloader):
                total_loss_test = 0
                q, h, t, s = test_batch
                predicted_y = self.model.predict(q, h, t)
                test_accuracy = self.measure_hit_binary(predicted_y, s)
                total_test_accuracy += test_accuracy
                hit_1_rate, hit_5_rate, hit_10_rate = self.rank_candidates(test_batch)
                total_hit_1_rate += hit_1_rate
                total_hit_5_rate += hit_5_rate
                total_hit_10_rate += hit_10_rate
            total_hit_1_rate = total_hit_1_rate / len(self.test_dataloader)
            total_hit_5_rate = total_hit_5_rate / len(self.test_dataloader)
            total_hit_10_rate = total_hit_10_rate / len(self.test_dataloader)
            print(f'total test accuracy {total_test_accuracy / len(self.test_dataloader)}')
            print(f'total_hit_1_rate:', total_hit_1_rate)
            print(f'total_hit_5_rate:', total_hit_5_rate)
            print(f'total_hit_10_rate:', total_hit_10_rate)

        self.save_model()

    def train(self):
        with open('training.log', 'w') as f:
            f.write('Began the training \n')
            f.close()
        total_loss_train_list = []
        total_train_accuracy_list = []
        for epoch in range(self.epoch_num):
            total_loss_train = 0
            total_train_accuracy = 0
            for train_batch in tqdm(self.train_dataloader):
                q, h, t, s = train_batch
                loss, predicted_y = self.model(q, h, t, s)
                loss.mean().cpu().backward()
                total_loss_train += loss
                self.optimizer.step()
                train_accuracy = self.measure_hit_binary(predicted_y, s)
                total_train_accuracy += train_accuracy

            total_train_accuracy = total_train_accuracy / len(self.train_dataloader)
            print(f'total loss train {total_loss_train}')
            print(f'total accuracy train {total_train_accuracy}')
            print(f'current learning rate {self.scheduler.get_last_lr()}')
            total_loss_train_list.append(total_loss_train)
            total_train_accuracy_list.append(total_train_accuracy)

            if (epoch) % 100 == 0:
                self.evaluate()
                self.scheduler.step()
                with open('training.log', 'a') as f:
                    f.write(f'Epoch number: {epoch}')
                    f.write(str(total_loss_train_list))
                    f.write(json.dumps(total_train_accuracy_list))
                    f.write('\n ----------------------------------------\n')

        return total_loss_train_list, total_train_accuracy_list


if __name__ == '__main__':
    # TODO: rename the file
    l_r = 1e-4
    my_trainer = Trainer(batch_size=32, epoch_num=1000, learning_rate=l_r)
    loss, accuracy = my_trainer.train()
    print(f"at learning rate of {l_r}, loss is : {loss}, accuracy is {accuracy}")
