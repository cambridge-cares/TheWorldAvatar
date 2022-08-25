import sys
sys.path.append('../../../')

from torch.optim.lr_scheduler import ExponentialLR
import os
import torch
from torch.utils import tensorboard
from tqdm import tqdm
from Marie.Util.location import DATA_DIR
from Marie.Util.Models.TransE import TransE
from Marie.Util.Models.TransE_Dataset import Dataset




# TODO: make the data builder based on the entity and rel mapping
# This serves as the standard template for dataset processing


class Trainer():
    def __init__(self):
        # previous optimal parameters , b = 128, l_r = 0.01 , dim = 50
        self.batch_size = 128
        self.epoches = 5000
        self.step = 0
        self.learning_rate = 10
        self.dim = 50
        self.dataset_name  = 'pubchem5000'

        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR, f'{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets = [line.split('\t') for line in
                         open(os.path.join(DATA_DIR, f'{self.dataset_name}-test.txt')).read().splitlines()]

        self.train_set = Dataset(train_triplets)
        self.test_set = Dataset(test_triplets)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num

        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        self.device = device
        print(f'=========== USING {device} ===============')
        self.model = TransE(dim=self.dim, ent_num=self.e_num, rel_num=self.r_num, resume_training=False, device=device)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=0.999)

        self.train_dataloader = torch.utils.data.DataLoader(self.train_set, batch_size=self.batch_size, shuffle=True)
        self.test_dataloader = torch.utils.data.DataLoader(self.test_set, batch_size=self.batch_size, shuffle=True)

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10):
        _, indices_top_k = torch.topk(predictions.to(self.device), k=k, largest=False)
        if ground_truth_idx.to(self.device) in indices_top_k.to(self.device):
            return 1
        else:
            return 0

    def train(self):

        with tqdm(total=self.epoches, unit=' epoch') as tepoch:

            for epoch_num in range(self.epoches):
                # init loss and accuracy numbers
                total_loss_train = 0
                tepoch.set_description(f"Epoch {epoch_num + 1} ")
                self.model.train()
                total_loss_train = 0

                for positive_triplets, negative_triples in tqdm(self.train_dataloader):
                    self.optimizer.zero_grad()
                    loss = self.model(positive_triplets, negative_triples)
                    loss.mean().backward()
                    loss = loss.data.cuda()
                    self.optimizer.step()
                    self.step += 1
                    total_loss_train += loss.mean().item()

                if epoch_num % 20 == 0:
                    self.evaluate()
                    self.save_model()

                tepoch.write(f"acc_loss_train: {total_loss_train}")

                self.scheduler.step()
                my_lr = self.scheduler.get_lr()
                tepoch.write(f"current learning rate : {my_lr}")

    def export_embeddings(self):
        ent_lines = []

        for embedding in self.model.ent_embedding.weight.data:
            e_line = '\t'.join([str(e) for e in embedding.tolist()])
            ent_lines.append(e_line)
        ent_content = '\n'.join(ent_lines)
        with open(os.path.join(DATA_DIR, 'ent_embedding.tsv'), 'w') as f:
            f.write(ent_content)
            f.close()

        rel_lines = []
        for embedding in self.model.rel_embedding.weight.data:
            r_line = '\t'.join([str(r) for r in embedding.tolist()])
            rel_lines.append(r_line)
        rel_content = '\n'.join(rel_lines)
        with open(os.path.join(DATA_DIR,'rel_embedding.tsv'), 'w') as f:
            f.write(rel_content)
            f.close()

    def save_model(self):
        torch.save(self.model.state_dict(), 'model')
        self.export_embeddings()
        print(f'saving the model and the embeddings')

    def evaluate(self):
        total_loss_val = 0
        hit_10 = 0
        hit_5 = 0
        hit_1 = 0
        total_case = 0

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
                hit_10 += self.hit_at_k(prediction.to(self.device), tail_true, k=10)
                hit_5 += self.hit_at_k(prediction.to(self.device), tail_true, k=5)
                hit_1 += self.hit_at_k(prediction.to(self.device), tail_true, k=1)

        print('Current Hit 10 rate:', hit_10, ' out of ', total_case, ' ratio is: ', hit_10 / total_case)
        print('Current Hit 5 rate:', hit_5, ' out of ', total_case, ' ratio is: ', hit_5 / total_case)
        print('Current Hit 1 rate:', hit_1, ' out of ', total_case, ' ratio is: ', hit_1 / total_case)
        print(f'total_loss_val {total_loss_val}')


if __name__ == '__main__':
    trainer = Trainer()
    trainer.train()
