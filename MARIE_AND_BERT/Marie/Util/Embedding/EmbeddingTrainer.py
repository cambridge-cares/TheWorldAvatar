import os
import random

import pandas as pd
import torch
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.Dataset.Complex_Dataset import ComplexDataset
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor


class Trainer:

    def __init__(self, model, dataset_name: str, epochs: int = 100, learning_rate: float = 0.01, gamma: float = 0.9,
                 data_folder='ontocompchem_calculation', save_model: bool = False, complex: bool = True,
                 pointwise: bool = False,
                 batch_size: int = 64, test_step: int = 200, neg_rate = 100, scheduler_step = 50):
        """
        :param model:
        :param dataset_name:
        :param epochs:
        :param learning_rate:
        :param data_folder:
        :param save_model:
        :param complex:
        :param pointwise: whether the training is pointwise (or pairwise)
        :param batch_size: the size of a batch
        """
        self.epochs = epochs
        self.dataset_name = dataset_name
        self.batch_size = batch_size
        self.learning_rate = learning_rate
        self.scheduler_step = scheduler_step
        self.gamma = gamma
        self.step = 0
        self.save_model = save_model
        self.complex = complex
        self.data_folder = data_folder
        self.pointwise = pointwise
        self.test_step = test_step
        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR,
                                            f'{data_folder}/{self.dataset_name}-train.txt')).read().splitlines()]
        test_triplets = random.sample(train_triplets, round(len(train_triplets) * 0.2))
        print("triples prepared")
        if self.pointwise:
            df_train = pd.read_csv(os.path.join(DATA_DIR,
                                                f'{data_folder}/{self.dataset_name}-train.txt'), sep="\t", header=None)
            df_test = pd.read_csv(os.path.join(DATA_DIR,
                                               f'{data_folder}/{self.dataset_name}-test.txt'), sep="\t", header=None)

            # df_train = df_train.sample(frac=0.1)
            # df_test = df_test.sample(frac=0.01)
            # df_train =
            self.train_set = ComplexDataset(df_train, data_folder=data_folder, dataset_name=self.dataset_name, mode="train", neg_rate = neg_rate)
            self.test_set = ComplexDataset(df_test, data_folder=data_folder, dataset_name=self.dataset_name, mode="test")
        else:
            self.train_set = Dataset(train_triplets, data_folder=data_folder)
            self.test_set = Dataset(test_triplets, data_folder=data_folder)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)
        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        self.model = model
        self.optimizer = torch.optim.Adam(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, self.data_folder),
                                          dataset_name=self.dataset_name)
        self.training_records = []

    def train(self):
        for epoch_num in tqdm(range(self.epochs)):
            self.model.train()
            total_loss_train = 0
            for pos_train, neg_train in tqdm(self.train_dataloader):
                """For pointwise training """
                self.optimizer.zero_grad()
                if self.pointwise:
                    loss = self.model(pos_train)
                else:
                    loss = self.model(pos_train, neg_train)

                loss.backward()
                loss = loss.data.cuda()
                self.optimizer.step()
                self.step += 1
                total_loss_train += loss.mean().item()
            print('total_loss_train: ', total_loss_train)
            print('current learning rate', self.scheduler.get_lr())

            if (epoch_num + 1) % self.scheduler_step == 0:
                self.scheduler.step()

            if (epoch_num + 1) % self.test_step == 0:
                if self.pointwise:
                    self.evaluate_pointwise(epoch=epoch_num)
                else:
                    self.evaluate_pairwise()
                if self.save_model:
                    self.export_embeddings()

        # if self.pointwise:
            # self.evaluate_pointwise(epoch=epoch_num)


    def k_hit_evaluation(self, largest=True, global_compare=False):
        total_counter = 0
        total_hit_1 = 0
        total_hit_5 = 0
        total_hit_10 = 0
        filtered_total_hit_1 = 0
        filtered_total_hit_5 = 0
        filtered_total_hit_10 = 0

        true_rank = 0
        f_true_rank = 0

        for positive_triplets, _ in tqdm(self.test_dataloader):
            ground_truth_triplets = torch.transpose(torch.stack(positive_triplets), 0, 1).type(torch.LongTensor)
            for i, triplet in enumerate(ground_truth_triplets):
                head = triplet[0]
                rel = triplet[1]
                tail_true = triplet[2]

                if global_compare:
                    head_tensor = head.repeat(self.e_num)
                    rel_tensor = rel.repeat(self.e_num)
                    tail_all = torch.arange(0, self.e_num).type(torch.LongTensor)
                    tail_true_idx = tail_all.tolist().index(tail_true.item())
                else:
                    tail_all = self.hop_extractor.extract_neighbour_from_idx(head.item())
                    if tail_true.item() not in tail_all:
                        tail_all.append(tail_true.item())
                    tail_true_idx = tail_all.index(tail_true.item())
                    true_idx = tail_true_idx
                    head_tensor = head.repeat(len(tail_all))
                    rel_tensor = rel.repeat(len(tail_all))
                    tail_all = torch.LongTensor(tail_all)

                tail_filtered = []
                for tail in tail_all:
                    # find the
                    triple_str = f"{head.item()}_{rel.item()}_{tail}"
                    if not self.hop_extractor.check_triple_existence(triple_str):
                        tail_filtered.append(tail)
                tail_filtered.append(tail_true.item())
                tail_true_idx_filterd = tail_filtered.index(tail_true.item())
                tail_all = torch.LongTensor(tail_all)
                head_filtered = head.repeat(len(tail_filtered))
                rel_filtered = rel.repeat(len(tail_filtered))
                tail_filtered = torch.LongTensor(tail_filtered)
                new_triplets = torch.stack((head_tensor, rel_tensor, tail_all)).type(torch.LongTensor)
                prediction = self.model.predict(new_triplets)

                filtered_triplets = torch.stack((head_filtered, rel_filtered, tail_filtered)).type(torch.LongTensor)
                prediction_filtered = self.model.predict(filtered_triplets)

                total_counter += 1
                total_hit_1 += self.hit_at_k(prediction, tail_true_idx, k=1, largest=largest)
                total_hit_5 += self.hit_at_k(prediction, tail_true_idx, k=5, largest=largest)
                total_hit_10 += self.hit_at_k(prediction, tail_true_idx, k=10, largest=largest)

                filtered_total_hit_1 += self.hit_at_k(prediction_filtered, tail_true_idx_filterd, k=1, largest=largest)
                filtered_total_hit_5 += self.hit_at_k(prediction_filtered, tail_true_idx_filterd, k=5, largest=largest)
                filtered_total_hit_10 += self.hit_at_k(prediction_filtered, tail_true_idx_filterd, k=10,
                                                       largest=largest)

                true_rank += self.get_rank(prediction, tail_true_idx, largest=largest)
                f_true_rank += self.get_rank(prediction_filtered, tail_true_idx_filterd, largest=largest)

        mrr = true_rank / total_counter
        f_mrr = f_true_rank / total_counter
        print('Current Hit 10 rate:', total_hit_10, ' out of ', total_counter, ' ratio is: ',
              total_hit_10 / total_counter)
        print('Current Hit 5 rate:', total_hit_5, ' out of ', total_counter, ' ratio is: ', total_hit_5 / total_counter)
        print('Current Hit 1 rate:', total_hit_1, ' out of ', total_counter, ' ratio is: ', total_hit_1 / total_counter)
        print('Current Filtered Hit 10 rate:', filtered_total_hit_10, ' out of ', total_counter, ' ratio is: ',
              filtered_total_hit_10 / total_counter)
        print('Current Filtered Hit 5 rate:', filtered_total_hit_5, ' out of ', total_counter, ' ratio is: ',
              filtered_total_hit_5 / total_counter)
        print('Current Filtered Hit 1 rate:', filtered_total_hit_1, ' out of ', total_counter, ' ratio is: ',
              filtered_total_hit_1 / total_counter)

        hit_1 = total_hit_1 / total_counter
        hit_5 = total_hit_5 / total_counter
        hit_10 = total_hit_10 / total_counter
        filtered_total_hit_1 = hit_1
        filtered_total_hit_5 = hit_5
        filtered_total_hit_10 = hit_10

        print("mmr: ", mrr)
        print("f_mmr: ", f_mrr)
        return [hit_1, hit_5, hit_10, filtered_total_hit_1, filtered_total_hit_5, filtered_total_hit_10, mrr, f_mrr]

    def get_rank(self, predictions, ground_truth_idx, largest=False):
        k = len(predictions)
        _, indices_top_k = torch.topk(predictions, k=k, largest=largest)
        rank = 1 / (indices_top_k.tolist().index(ground_truth_idx) + 1)
        return rank

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10, largest=False):
        k = min(k, len(predictions))
        _, indices_top_k = torch.topk(predictions, k=k, largest=largest)
        if ground_truth_idx in indices_top_k:
            return 1
        else:
            return 0

    def export_embeddings(self):
        if self.complex:
            ent_lines = []
            for re_ent, im_ent in zip(self.model.re_ent.weight.data, self.model.im_ent.weight.data):
                e_line = '\t'.join([str(e) for e in re_ent.tolist()] + [str(e) for e in im_ent.tolist()])
                ent_lines.append(e_line)

            ent_content = '\n'.join(ent_lines)
            with open(os.path.join(DATA_DIR, f'{self.data_folder}/ent_embedding.tsv'), 'w') as f:
                f.write(ent_content)
                f.close()

            rel_lines = []
            for re_rel, im_rel in zip(self.model.re_rel.weight.data, self.model.im_rel.weight.data):
                r_line = '\t'.join([str(e) for e in re_rel.tolist()] + [str(e) for e in im_rel.tolist()])
                rel_lines.append(r_line)
            rel_content = '\n'.join(rel_lines)
            with open(os.path.join(DATA_DIR, f'{self.data_folder}/rel_embedding.tsv'), 'w') as f:
                f.write(rel_content)
                f.close()

    def evaluate_pointwise(self, epoch):
        with torch.no_grad():
            total_pos_acc = 0
            total_neg_acc = 0
            for positive_triplets, negative_triplets in self.test_dataloader:
                pos_pre = self.model.predict(positive_triplets)  # should all be > 0
                pos_count = torch.numel(pos_pre[pos_pre > 0])
                pos_acc = pos_count / len(pos_pre)
                total_pos_acc += pos_acc
                neg_pre = self.model.predict(negative_triplets)  # should all be < 0
                neg_count = torch.numel(neg_pre[neg_pre < 0])
                neg_acc = neg_count / len(neg_pre)
                total_neg_acc += neg_acc
            average_prediction_pos = total_pos_acc / len(self.test_dataloader)
            average_prediction_neg = total_neg_acc / len(self.test_dataloader)
            print(f'average prediction accuracy for positive {average_prediction_pos}')
            print(f'average prediction accuracy for negative {average_prediction_neg}')

            row = self.k_hit_evaluation(largest=True, global_compare=False)
            row.append(epoch)
            self.training_records.append(row)
            df = pd.DataFrame(self.training_records)
            df.columns = ['hit_1', 'hit_5', 'hit_10', 'f_hit_1', 'f_hit_5', 'f_hit_10', 'mrr', 'f_mrr', 'epoch']
            df.to_csv(os.path.join(DATA_DIR, self.data_folder, 'training_records.csv'), sep=',')


    def evaluate_pairwise(self):
        total_loss_val = 0
        self.model.eval()
        with torch.no_grad():
            for positive_triplets, _ in self.test_dataloader:
                prediction = self.model.predict(positive_triplets).mean()
                total_loss_val += prediction
            self.k_hit_evaluation()
        print(f'total_loss_val {total_loss_val}')
