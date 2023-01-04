import os

import torch
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

from Marie.Util.Dataset.TransEA_Dataset import Dataset
from Marie.Util.Models.TransEA import TransEA
from Utils.location import DATA_DIR


class TransEATrainer:
    """
    TransEA training comes with three parts of datasets: postive sets, negative sets, and attribute sets - all
    created on top of triples.
    for certain batches of positive sets,
    """

    def __init__(self, dataset_path, dataset_name, dim, epoch_num, learning_rate=1, gamma=1, batch_size = 128):
        self.dataset_path = dataset_path
        self.dataset_name = dataset_name
        self.dim = dim
        self.batch_size = batch_size
        self.epoch_num = epoch_num
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.step = 0
        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR, self.dataset_path,
                                            f'{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets = [line.split('\t') for line in
                         open(os.path.join(DATA_DIR, self.dataset_path,
                                           f'{self.dataset_name}-test.txt')).read().splitlines()]

        self.train_set = Dataset(train_triplets, dataset_path=self.dataset_path)
        self.test_set = Dataset(test_triplets, dataset_path=self.dataset_path)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        self.use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        print(f'=========== USING {device} ===============')
        self.model = TransEA(dim=self.dim, ent_num=self.e_num, rel_num=self.r_num,
                             resume_training=False, device=device, dataset_path=dataset_path)
        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

    def get_train_data(self):
        train_triplets_numerical = [line.split('\t') for line in
                                    open(os.path.join(DATA_DIR, self.dataset_path,
                                                      f'{self.dataset_name}_numerical-train.txt')).read().splitlines()]

        test_triplets_numerical = [line.split('\t') for line in
                                   open(os.path.join(DATA_DIR, self.dataset_path,
                                                     f'{self.dataset_name}_numerical-test.txt')).read().splitlines()]

        train_numerical_set = Dataset(train_triplets_numerical, dataset_path=self.dataset_path, is_numerical=True)
        test_numerical_set = Dataset(test_triplets_numerical, dataset_path=self.dataset_path, is_numerical=True)
        train_numerical_dataloader = torch.utils.data.DataLoader(train_numerical_set, batch_size=self.batch_size, shuffle=True)
        test_numerical_dataloader = torch.utils.data.DataLoader(test_numerical_set, batch_size=self.batch_size, shuffle=True)

        train_triplets_non_numerical = [line.split('\t') for line in
                                        open(os.path.join(DATA_DIR, self.dataset_path,
                                                          f'{self.dataset_name}_non_numerical-train.txt')).read().splitlines()]

        test_triplets_non_numerical = [line.split('\t') for line in
                                       open(os.path.join(DATA_DIR, self.dataset_path,
                                                         f'{self.dataset_name}_non_numerical-test.txt')).read().splitlines()]

        train_non_numerical_set = Dataset(train_triplets_non_numerical, dataset_path=self.dataset_path,
                                          is_numerical=False)
        test_non_numerical_set = Dataset(test_triplets_non_numerical, dataset_path=self.dataset_path,
                                         is_numerical=False)
        train_non_numerical_dataloader = torch.utils.data.DataLoader(train_non_numerical_set, batch_size=self.batch_size,
                                                                     shuffle=True)
        test_non_numerical_dataloader = torch.utils.data.DataLoader(test_non_numerical_set, batch_size=self.batch_size, shuffle=True)

        return train_numerical_dataloader, train_non_numerical_dataloader, \
               test_numerical_dataloader, test_non_numerical_dataloader

    def run(self):
        train_numerical_dataloader, test_numerical_dataloader, \
        train_non_numerical_dataloader, test_non_numerical_dataloader = self.get_train_data()

        # train numerical datasets first

        for epoch in tqdm(range(self.epoch_num)):
            total_loss_train = 0
            for pos_triples, neg_triples, numerical_list in train_numerical_dataloader:
                loss = self.model(positive_triplets=pos_triples, negative_triplets=neg_triples,
                                  numerical_list=numerical_list, is_numerical=True)
                loss.backward()
                total_loss_train += loss.mean().item()
                self.optimizer.step()
                self.step += 1
            print(total_loss_train)


if __name__ == "__main__":
    trainer = TransEATrainer(dataset_path="CrossGraph/wikidata_single",
                             dataset_name="wikidata_single", dim=10, epoch_num=5000)
    trainer.run()
