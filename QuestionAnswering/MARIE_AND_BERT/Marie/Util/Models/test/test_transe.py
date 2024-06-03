import os
import sys
import unittest
sys.path.append('../../../')

from Marie.Util.Models.ModelBuilder_Torch_TransE import Dataset
from Marie.Util.location import DATASET_DIR
from Marie.Util.Models.TransE import TransE


class MyTestCase(unittest.TestCase):
    def test_load_embedding(self):
        data_path = os.path.join(DATASET_DIR, 'PubchemMiniFull')

        train_triplets = [line.split('\t') for line in
                          open(os.path.join(data_path, 'pubchemini-train.txt')).read().splitlines()]

        test_triplets = [line.split('\t') for line in
                         open(os.path.join(data_path, 'pubchemini-test.txt')).read().splitlines()]

        self.train_set = Dataset(train_triplets)
        self.test_set = Dataset(test_triplets)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        model = TransE(dim=50, ent_num=self.e_num, rel_num=self.r_num)
        model.load_ent_embedding()
        print(model.ent_embedding(1))


if __name__ == '__main__':
    unittest.main()