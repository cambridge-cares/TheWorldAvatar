import json
import random
import sys

sys.path.append('../../')

from torch.optim.lr_scheduler import ExponentialLR
import os
import torch
from tqdm import tqdm
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.TransE import TransE
from Marie.Util.Dataset.TransE_Dataset import Dataset


class Trainer:
    def __init__(self, batch_size=1024, epochs=5000, learning_rate=1, gamma = 1, dataset_path="pubchem", dataset_name='pubchem500',
                 dim=20, mode='production', load_pretrained_embeddings=False):
        self.batch_size = batch_size
        self.epochs = epochs
        self.step = 0
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.dim = dim
        self.dataset_name = dataset_name
        self.dataset_path = dataset_path
        self.mode = mode

        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR, self.dataset_path, f'{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets = [line.split('\t') for line in
                         open(os.path.join(DATA_DIR, self.dataset_path, f'{self.dataset_name}-test.txt')).read().splitlines()]

        self.train_set = Dataset(train_triplets, dataset_path=self.dataset_path)
        self.test_set = Dataset(test_triplets, dataset_path=self.dataset_path)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num

        self.use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        print(f'=========== USING {device} ===============')
        self.model = TransE(dim=self.dim, ent_num=self.e_num, rel_num=self.r_num,
                            resume_training=load_pretrained_embeddings, device=device, dataset_path = dataset_path)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=32, shuffle=True)

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10):
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=False)
        if ground_truth_idx.to(self.device) in indices_top_k.to(self.device):
            return 1
        else:
            return 0

    def train(self):

        with tqdm(total=self.epochs, unit=' epoch') as tepoch:

            for epoch_num in range(self.epochs):
                # init loss and accuracy numbers
                tepoch.set_description(f"Epoch {epoch_num + 1} ")
                self.model.train()
                total_loss_train = 0

                for positive_triplets, negative_triples in tqdm(self.train_dataloader):
                    self.optimizer.zero_grad()
                    loss = self.model(positive_triplets, negative_triples)
                    loss.mean().backward()
                    if self.use_cuda:
                        loss = loss.data.cuda()
                    else:
                        loss = loss.data.cpu()
                    self.optimizer.step()
                    self.step += 1
                    total_loss_train += loss.mean().item()
                tepoch.write(f"acc_loss_train: {total_loss_train}")
                tepoch.write(f"current learning rate : {self.learning_rate}")

                if epoch_num % 20 == 0:
                    self.evaluate()
                    self.save_model()

                self.scheduler.step()
                self.learning_rate = self.scheduler.get_lr()


    def export_embeddings(self):
        ent_lines = []

        for embedding in self.model.ent_embedding.weight.data:
            e_line = '\t'.join([str(e) for e in embedding.tolist()])
            ent_lines.append(e_line)
        ent_content = '\n'.join(ent_lines)
        with open(os.path.join(DATA_DIR, self.dataset_path, 'ent_embedding.tsv'), 'w') as f:
            f.write(ent_content)
            f.close()

        rel_lines = []
        for embedding in self.model.rel_embedding.weight.data:
            r_line = '\t'.join([str(r) for r in embedding.tolist()])
            rel_lines.append(r_line)
        rel_content = '\n'.join(rel_lines)
        with open(os.path.join(DATA_DIR, self.dataset_path, 'rel_embedding.tsv'), 'w') as f:
            f.write(rel_content)
            f.close()

    def save_model(self):
        if self.mode == 'test':
            print('In test mode, embeddings will not be saved')
        else:
            self.export_embeddings()
            print(f'saving the embeddings')

    def evaluate(self):
        total_loss_val = 0
        hit_10 = 0
        hit_5 = 0
        hit_1 = 0
        total_case = 0

        for positive_triplets, _ in tqdm(random.sample(list(self.test_dataloader), 20)):
            # for positive_triplets, _ in tqdm(self.test_dataloader):
            prediction = self.model.predict(positive_triplets).mean()
            total_loss_val += prediction
            ground_truth_triplets = torch.transpose(torch.stack(positive_triplets), 0, 1).type(torch.LongTensor)
            # for i, triplet in enumerate(random.sample(list(ground_truth_triplets), 30)):
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
                hit_10 += self.hit_at_k(prediction.to(self.device), tail_true, k=10)
                hit_5 += self.hit_at_k(prediction.to(self.device), tail_true, k=5)
                hit_1 += self.hit_at_k(prediction.to(self.device), tail_true, k=1)

                # replace the make a list of stuff
        '''
        Add function to find out the hit rate within the subgraph 
        '''
        print('=======================================================================================================')
        print('Current Hit 10 rate:', hit_10, ' out of ', total_case, ' ratio is: ', hit_10 / total_case)
        print('Current Hit 5 rate:', hit_5, ' out of ', total_case, ' ratio is: ', hit_5 / total_case)
        print('Current Hit 1 rate:', hit_1, ' out of ', total_case, ' ratio is: ', hit_1 / total_case)
        print(f'total_loss_val {total_loss_val}')
        return hit_1 / total_case, total_loss_val


if __name__ == "__main__":
    my_transe_trainer = Trainer(dataset_path="CrossGraph/ontokin", dataset_name="ontokin",
                                load_pretrained_embeddings=True, dim=40, batch_size=32)
    my_transe_trainer.train()
