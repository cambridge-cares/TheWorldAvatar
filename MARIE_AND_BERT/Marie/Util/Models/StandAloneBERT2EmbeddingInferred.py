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
        self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
        self.df = df
        self.max_len = 12
        self.tokenized_questions = [self.tokenizer(text,
                                                   padding='max_length', max_length=self.max_len, truncation=True,
                                                   return_tensors="pt") for text in self.df['question']]

        embedding_path = os.path.join(DATA_DIR, 'ontocompchem_latent_40')
        self.ent_embedding = pd.read_csv(os.path.join(embedding_path, 'ent_embedding.tsv'), sep='\t', header=None)
        self.rel_embedding = pd.read_csv(os.path.join(embedding_path, 'rel_embedding.tsv'), sep='\t', header=None)
        self.heads = self.df["head"].tolist()
        self.tails = self.df["tail"].tolist()
        self.questions = self.df['question'].tolist()
        self.y = self.df["rel"]

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

    def __init__(self, ent_embedding=None, rel_embedding=None, for_training=False, device=torch.device("cpu"),
                 tail_rel_embedding=None, idx2entity=None, load_model=False, model_name=None, dataset_dir=None):
        super(StandAloneBERT, self).__init__()
        self.device = device
        self.bert = BertModel.from_pretrained('bert-base-uncased')
        self.dropout = nn.Dropout(0)
        self.dropout_relation = nn.Dropout(0.1)
        self.linear = nn.Linear(768, 80)  # keep this model ...
        self.sigmoid = nn.Sigmoid()
        # self.for_training = for_training
        # if self.for_training:
        #     self.model_dir = TRAINING_DIR
        # else:
        #     self.model_dir = DEPLOYMENT_DIR

        embedding_dim = int(ent_embedding.shape[1])
        half = int(embedding_dim / 2)
        self.ent_embedding = ent_embedding
        self.rel_embedding = rel_embedding
        self.re_ent = ent_embedding.iloc[:, :half]  # first half of the embedding
        self.im_ent = ent_embedding.iloc[:, half:]
        self.re_rel = rel_embedding.iloc[:, :half]
        self.im_rel = rel_embedding.iloc[:, half:]
        self.half = half
        # self.criterion = nn.BCELoss()
        self.tail_rel_embedding = tail_rel_embedding
        self.idx2entity = idx2entity
        self.rel_factor = 10
        # TODO: load the embeddings and divide them into two parts re and im


        if dataset_dir is not None:
            self.dataset_dir = dataset_dir

        if load_model:
            self.load_model(model_name)

    def load_model(self, model_name):
        print(" - Loading pretrained BERT Mapping model")
        self.load_state_dict(torch.load(os.path.join(self.dataset_dir, model_name), map_location=self.device))

    def pointwise_bce(self, preds, target):
        """
        :param preds: model predicted score of the triple
        :param target: true score of the triple, which is a binary value 0 or 1 ...
        :return: Binary Cross Enthropy with Logits loss (Built in sigmoid activation)
        """
        preds = torch.clamp(preds, min=0, max=1)
        loss = torch.nn.BCEWithLogitsLoss()(preds, target)
        return loss

    def score(self, triple, single=False):
        head_idx = triple[0]
        predicted_rel = triple[1]
        tail_idx = triple[2]
        re_head = torch.FloatTensor(self.re_ent.iloc[head_idx].values.tolist()).to(self.device)
        im_head = torch.FloatTensor(self.im_ent.iloc[head_idx].values.tolist()).to(self.device)

        if single:
            re_rel, im_rel = torch.split(predicted_rel, self.half)
        else:
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

        true_rel = torch.FloatTensor(self.rel_embedding.iloc[y].values.tolist()).to(self.device)
        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        triple_score = self.score((head, predicted_rel, tail)).to(self.device)

        distance = (predicted_rel - true_rel).norm(p=1, dim=1)

        return distance, triple_score

    def predict(self, question, head, tail, debug=False):
        input_ids = torch.reshape(question['input_ids'], (-1, max_len)).to(self.device)
        attention_mask = torch.reshape(question['attention_mask'], (-1, max_len)).to(self.device)
        pooled_output = self.bert(input_ids=input_ids,
                                  attention_mask=attention_mask,
                                  return_dict=False)[1].to(self.device)

        dropout_output = self.dropout(pooled_output).to(self.device)
        predicted_rel = self.linear(dropout_output).to(self.device)
        if debug:
            print("pooled_output", pooled_output[0])
            print("input_ids", input_ids[0])
            print("predicted rel", predicted_rel[0])

        triple_score = self.score((head, predicted_rel, tail)).to(self.device)
        return triple_score


class Trainer():

    def __init__(self, batch_size=64, epoch_num=100, learning_rate=0.1, gamma=0.1):
        self.batch_size = batch_size
        self.epoch_num = epoch_num
        self.learning_rate = learning_rate
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        self.dataset_dir = "ontocompchem_latent_40"
        print(f'=========== USING {self.device} ===============')

        dataset_path = os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv')
        df = pd.read_csv(dataset_path, sep="\t")
        df = df.sample(frac=0.1)
        df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.8 * len(df))])
        dataset_full = Dataset(df, self.dataset_dir)
        dataset_test = Dataset(df_test, self.dataset_dir)
        dataset_train = Dataset(df_train, self.dataset_dir)
        self.test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        self.train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        self.ent_embedding = dataset_full.ent_embedding
        self.rel_embedding = dataset_full.rel_embedding
        self.tail_rel_embedding = None  # dataset_full.tail_rel_embedding
        self.ent_embedding_num = self.ent_embedding.shape[0]
        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                          dataset_name="ontocompchem_calculation")
        self.model = StandAloneBERT(device=self.device, ent_embedding=self.ent_embedding,
                                    rel_embedding=self.rel_embedding, for_training=True,
                                    tail_rel_embedding=self.tail_rel_embedding,
                                    idx2entity=self.hop_extractor.entity_labels).cuda()
        self.optimizer = torch.optim.Adam(self.model.parameters(), lr=self.learning_rate)
        self.model_name = "score_model_general"
        self.model_path = os.path.join(DATA_DIR, self.dataset_dir, self.model_name)
        # self.load_model()
        self.gamma = gamma
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

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
        # TODO: do it the numpy
        predicted_y = predicted_y.cpu().detach().numpy()
        predicted_y[predicted_y > 0.5] = 1
        predicted_y[predicted_y <= 0.5] = 0
        true_y = true_y.cpu().detach().numpy()
        count = np.sum(predicted_y == true_y)
        rate = count / len(true_y)
        return rate

    def extract_n_hop_subgraph(self, head, hop=3):
        pass

    def hit_at_k(self, predictions, ground_truth_idx=0, k: int = 10, all_tails=None):
        k = min(len(predictions), k)
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=True)
        if all_tails is not None:
            for idx in indices_top_k:
                score = predictions[idx]
                idx = all_tails[idx].item()
                l = self.hop_extractor.entity_labels[idx]
                # print('predicted label: ', l, 'score: ', score)
        # print('--------------')
        if ground_truth_idx in indices_top_k.to(self.device):
            return 1
        else:
            return 0

    def rank_candidates(self, batch):
        question, head, tail, score = batch
        # print(batch)
        input_ids = question['input_ids']
        attention_mask = question['attention_mask']
        hit_1_count = 0
        hit_5_count = 0
        hit_10_count = 0
        counter = 0
        for i_i, a_m, h, t, s in zip(input_ids, attention_mask, head, tail, score):
            with no_grad():
                # print(s)
                # print(s == 1)
                if 1 == 1:
                    # repeat the tensor n many times
                    all_tails = self.hop_extractor.extract_neighbour_from_idx(h.item())
                    t = t.item()
                    # print("==========================================================")
                    all_tails = list(filter(lambda a: a != t, all_tails))
                    all_tails.append(t)
                    true_idx = all_tails.index(t)
                    all_tails = torch.LongTensor(all_tails)
                    i_i_batch = i_i.repeat(len(all_tails), 1)
                    a_m_batch = a_m.repeat(len(all_tails), 1)
                    h = h.repeat(len(all_tails))
                    q = {"input_ids": i_i_batch, "attention_mask": a_m_batch}
                    prediction = self.model.predict(q, h, all_tails, s, debug=False)
                    hit_1_count += self.hit_at_k(prediction, k=1, ground_truth_idx=true_idx)
                    hit_5_count += self.hit_at_k(prediction, k=5, ground_truth_idx=true_idx)
                    hit_10_count += self.hit_at_k(prediction, k=len(prediction), ground_truth_idx=true_idx,
                                                  all_tails=all_tails)
                    counter += 1

        if counter == 0:
            return 0, 0, 0
        hit_1_rate = hit_1_count / counter
        hit_5_rate = hit_5_count / counter
        hit_10_rate = hit_10_count / counter
        return hit_1_rate, hit_5_rate, hit_10_rate

    def evaluate(self):
        # self.model.eval()

        total_hit_1_rate = 0
        total_hit_5_rate = 0
        total_hit_10_rate = 0
        total_test_accuracy = 0
        counter = 0
        with no_grad():
            for test_batch in tqdm(self.test_dataloader):
                total_loss_test = 0
                q, h, t, s = test_batch
                # predicted_y = self.model.predict(q, h, t, s)
                # test_accuracy = self.measure_hit_binary(predicted_y, s)
                # total_test_accuracy += test_accuracy
                hit_1_rate, hit_5_rate, hit_10_rate = self.rank_candidates(test_batch)
                total_hit_1_rate += hit_1_rate
                total_hit_5_rate += hit_5_rate
                total_hit_10_rate += hit_10_rate
                if hit_10_rate == 0:
                    pass
                else:
                    counter += 1

            total_hit_1_rate = total_hit_1_rate / counter
            total_hit_5_rate = total_hit_5_rate / counter
            total_hit_10_rate = total_hit_10_rate / counter
            # print(f'total test accuracy {total_test_accuracy / len(self.test_dataloader)}')
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
                loss.mean().backward()
                total_loss_train += loss.mean()
                self.optimizer.step()
                train_accuracy = self.measure_hit_binary(predicted_y, s)
                total_train_accuracy += train_accuracy

            total_train_accuracy = total_train_accuracy / len(self.train_dataloader)
            print(f'total loss train {total_loss_train}')
            print(f'total accuracy train {total_train_accuracy}')
            print(f'current learning rate {self.scheduler.get_last_lr()}')
            total_loss_train_list.append(total_loss_train)
            total_train_accuracy_list.append(total_train_accuracy)

            if (epoch) % 50 == 0:
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
    l_r = 1e-5
    epoch_num = 501
    batch_size = 32
    gamma = 0.5
    print(f"learning rate: {l_r}, epoch_num: {epoch_num}, batch_size: {batch_size}, gamma: {gamma}")
    my_trainer = Trainer(batch_size=batch_size, epoch_num=epoch_num, learning_rate=l_r, gamma=gamma)
    loss, accuracy = my_trainer.train()
    print(f"at learning rate of {l_r}, loss is : {loss}, accuracy is {accuracy}")
