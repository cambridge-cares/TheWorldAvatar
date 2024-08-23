import json
import sys
from torch.optim.lr_scheduler import ExponentialLR
import numpy as np
import torch
from torch import no_grad
import os
import pandas as pd
from tqdm import tqdm
sys.path.append("../..")
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor
from Marie.Util.Dataset.OntoScore_Dataset import Dataset
from Marie.Util.Models.ComplexScoreModel import ComplexScoreModel


class Trainer():

    def __init__(self, batch_size=64, epoch_num=100, learning_rate=0.1, gamma=0.1, dataset_dir = None, dataset_name = None):
        self.batch_size = batch_size
        self.epoch_num = epoch_num
        self.learning_rate = learning_rate
        use_cuda = torch.cuda.is_available()
        self.device = torch.device("cuda" if use_cuda else "cpu")
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        print(f'=========== USING {self.device} ===============')

        dataset_path = os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv')
        df = pd.read_csv(dataset_path, sep="\t")
        df = df.sample(frac=1)
        df_train, df_test = np.split(df.sample(frac=1, random_state=11), [int(.99 * len(df))])
        dataset_full = Dataset(df, self.dataset_dir)
        dataset_test = Dataset(df_test, self.dataset_dir)
        dataset_train = Dataset(df_train, self.dataset_dir)
        self.test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        self.train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        self.ent_embedding = dataset_full.ent_embedding
        self.rel_embedding = dataset_full.rel_embedding
        self.ent_embedding_num = self.ent_embedding.shape[0]
        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, self.dataset_dir),
                                          dataset_name=self.dataset_name)
        self.model = ComplexScoreModel(device=self.device, ent_embedding=self.ent_embedding,
                                       rel_embedding=self.rel_embedding, for_training=True,
                                       idx2entity=self.hop_extractor.entity_labels, load_model=False,
                                       dataset_dir=self.dataset_dir,
                                       model_name='score_model_general')
        if use_cuda:
            self.model.cuda()

        self.model_name = "score_model_general"
        self.model_path = os.path.join(DATA_DIR, self.dataset_dir, self.model_name)
        # self.model.load_pretrained_model(self.model_path)
        print(self.model_path)
        self.gamma = gamma
        self.optimizer = torch.optim.Adam(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

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

    def hit_at_k(self, predictions, ground_truth_idx=0, k: int = 10, all_tails=None):
        k = min(len(predictions), k)
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=True)
        if all_tails is not None:
            for idx in indices_top_k:
                score = predictions[idx]
                idx = all_tails[idx].item()
                l = self.hop_extractor.entity_labels[idx]

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
        counter = 0
        for i_i, a_m, h, t in zip(input_ids, attention_mask, head, tail):
            with no_grad():
                all_tails = self.hop_extractor.extract_neighbour_from_idx(h.item())
                t = t.item()
                # print("==========================================================")
                # print(self.hop_extractor.entity_labels[t])
                # print(self.hop_extractor.entity_labels[h.item()])
                all_tails = list(filter(lambda a: a != t, all_tails))
                all_tails.append(t)
                true_idx = all_tails.index(t)
                all_tails = torch.LongTensor(all_tails)
                i_i_batch = i_i.repeat(len(all_tails), 1)
                a_m_batch = a_m.repeat(len(all_tails), 1)
                h = h.repeat(len(all_tails))
                q = {"input_ids": i_i_batch, "attention_mask": a_m_batch}
                prediction = self.model.predict(q, h, all_tails, debug=False)
                hit_1_count += self.hit_at_k(prediction, k=1, ground_truth_idx=true_idx)
                hit_5_count += self.hit_at_k(prediction, k=5, ground_truth_idx=true_idx)
                hit_10_count += self.hit_at_k(prediction, k=len(prediction), ground_truth_idx=true_idx,
                                              all_tails=all_tails)
                counter += 1

        hit_1_rate = hit_1_count / counter
        hit_5_rate = hit_5_count / counter
        hit_10_rate = hit_10_count / counter
        return hit_1_rate, hit_5_rate, hit_10_rate

    def prepare_prediction_batch(self, question, head_entity, candidate_entities):
        """
        :param question: question in text
        :param head_entity: head entity index
        :param candidate_entities: list of candidate entity index
        :return: Ranked list of candidate entities
        """
        candidate_entities = torch.LongTensor(candidate_entities).to(self.device)
        repeat_num = len(candidate_entities)
        tokenized_question_batch = self.tokenize_question(question, repeat_num)
        head_entity_batch = torch.LongTensor([head_entity]).repeat(repeat_num).to(self.device)
        prediction_batch = (tokenized_question_batch, head_entity_batch, candidate_entities)
        return prediction_batch



    def evaluate(self):
        total_hit_1_rate = 0
        total_hit_5_rate = 0
        total_hit_10_rate = 0
        total_test_accuracy = 0
        counter = 0
        with no_grad():
            for test_batch in tqdm(self.test_dataloader):
                total_loss_test = 0
                q, h, t, s = test_batch
                predicted_y = self.model.predict(q, h, t, s)
                test_accuracy = self.measure_hit_binary(predicted_y, s)
                total_test_accuracy += test_accuracy
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

        # self.save_model()

    def train(self):
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

    def run(self):
        with open('training.log', 'w') as f:
            f.write('Began the training \n')
            f.close()

        for epoch in range(self.epoch_num):
            self.train()
            if (epoch + 1) % 100 == 0:
                self.evaluate()


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--ontology", help="main ontology used")
    args = parser.parse_args()
    ontology = "ontocompchem"
    if args.ontology:
        ontology = args.ontology
    l_r = 1e-5
    epoch_num = 501
    batch_size = 32
    gamma = 0.5
    print(f"learning rate: {l_r}, epoch_num: {epoch_num}, batch_size: {batch_size}, gamma: {gamma}")
    my_trainer = Trainer(batch_size=batch_size, epoch_num=epoch_num, learning_rate=l_r, gamma=gamma, dataset_dir=f"CrossGraph/{ontology}", dataset_name=ontology)
    my_trainer.run()
