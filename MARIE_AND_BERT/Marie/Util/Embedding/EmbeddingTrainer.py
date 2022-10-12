import os
import random
import torch
from tqdm import tqdm

from Marie.Util.Dataset.TransE_Dataset import Dataset
from Marie.Util.location import DATA_DIR


class Trainer:

    def __init__(self, model, dataset_name, epochs=100, learning_rate=0.01,
                 data_folder='ontocompchem_calculation', save_model=False, complex=True, pointwise=False, batch_size = 64):
        self.epochs = epochs
        self.dataset_name = dataset_name
        self.batch_size = batch_size
        self.learning_rate = learning_rate
        self.step = 0
        self.save_model = save_model
        self.complex = complex
        self.data_folder = data_folder
        self.pointwise = pointwise

        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR,
                                            f'{data_folder}/{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets = random.sample(train_triplets, round(len(train_triplets) * 0.2))
        self.train_set = Dataset(train_triplets, data_folder=data_folder)
        self.test_set = Dataset(test_triplets, data_folder=data_folder)
        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        # self.model = model_class(ent_num=self.e_num, rel_num=self.r_num, emb_dim=50)
        self.model = model

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)

    def train(self):
        for epoch_num in tqdm(range(self.epochs)):
            self.model.train()
            total_loss_train = 0
            for pos_train, neg_train in tqdm(self.train_dataloader):
                """For pointwise training """
                self.optimizer.zero_grad()

                if self.pointwise:
                    # give 1 for pos_train, 0 for neg_train
                    pos_labels = torch.LongTensor([1]).repeat(len(pos_train[0]))
                    pos_train.append(pos_labels)
                    neg_labels = torch.LongTensor([0]).repeat(len(neg_train[0]))
                    neg_train.append(neg_labels)
                    pos_train = torch.stack(pos_train)
                    neg_train = torch.stack(neg_train)
                    whole_train = torch.cat([pos_train, neg_train], dim=1)
                    loss = self.model(whole_train)
                else:
                    loss = self.model(pos_train, neg_train)
                loss.mean().backward()
                loss = loss.data.cuda()
                self.optimizer.step()
                self.step += 1
                total_loss_train += loss.mean().item()
            print('total_loss_train: ', total_loss_train)

            if epoch_num % 20 == 0:
                if self.pointwise:
                    self.evaluate_pointwise()
                else:
                    self.evaluate_pairwise()
                if self.save_model:
                    self.export_embeddings()

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10):
        _, indices_top_k = torch.topk(predictions, k=k, largest=False)
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


    def evaluate_pointwise(self):
        with torch.no_grad():
            total_pos_acc = 0
            total_neg_acc = 0
            for positive_triplets, negative_triplets in self.test_dataloader:
                pos_pre = self.model.predict(positive_triplets)  # should all be > 0
                pos_count = torch.numel(pos_pre[pos_pre > 0])
                pos_acc = pos_count / len(pos_pre)
                total_pos_acc += pos_acc

                neg_pre = self.model.predict(negative_triplets)  # should all be > 0
                neg_count = torch.numel(neg_pre[neg_pre < 0])
                neg_acc = neg_count / len(neg_pre)
                total_neg_acc += neg_acc

        #         total_mean_pos += self.model.predict(positive_triplets).mean()
        #         total_mean_neg += self.model.predict(negative_triplets).mean()
        #
            average_prediction_pos = total_pos_acc / len(self.test_dataloader)
            average_prediction_neg = total_neg_acc / len(self.test_dataloader)
            print(f'average prediction accuracy for positive {average_prediction_pos}')
            print(f'average prediction accuracy for negative {average_prediction_neg}')


    def evaluate_pairwise(self):
        total_loss_val = 0
        hit_10 = 0
        hit_5 = 0
        hit_1 = 0
        total_case = 0
        self.model.eval()
        with torch.no_grad():
            for positive_triplets, _ in self.test_dataloader:
                prediction = self.model.predict(positive_triplets).mean()
                total_loss_val += prediction

                ground_truth_triplets = torch.transpose(torch.stack(positive_triplets), 0, 1).type(torch.LongTensor)
                for i, triplet in enumerate(ground_truth_triplets):
                    head = triplet[0]
                    rel = triplet[1]
                    tail_true = triplet[2]
                    head_tensor = head.repeat(self.e_num)
                    rel_tensor = rel.repeat(self.e_num)
                    tail_all = torch.range(0, self.e_num - 1).type(torch.LongTensor)
                    new_triplets = torch.stack((head_tensor, rel_tensor, tail_all)).type(torch.LongTensor)
                    prediction = self.model.predict(new_triplets)

                    total_case += 1
                    hit_10 += self.hit_at_k(prediction, tail_true, k=10)
                    hit_5 += self.hit_at_k(prediction, tail_true, k=5)
                    hit_1 += self.hit_at_k(prediction, tail_true, k=1)

        print('Current Hit 10 rate:', hit_10, ' out of ', total_case, ' ratio is: ', hit_10 / total_case)
        print('Current Hit 5 rate:', hit_5, ' out of ', total_case, ' ratio is: ', hit_5 / total_case)
        print('Current Hit 1 rate:', hit_1, ' out of ', total_case, ' ratio is: ', hit_1 / total_case)
        print(f'total_loss_val {total_loss_val}')
