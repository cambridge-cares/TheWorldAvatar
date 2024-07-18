from Training.Embedding.TransE_Trainer import Trainer

import unittest


class MyTestCase(unittest.TestCase):

    def test_transe_trainer(self):
        my_trainer = Trainer(mode='test', load_pretrained_embeddings=True)
        my_trainer.dataset_name = 'pubchem500'
        assert (my_trainer.device.type == 'cuda')
        hit_1_ratio, val_loss = my_trainer.evaluate()
        print('hit_1_ratio', hit_1_ratio)
        assert (hit_1_ratio >= 0.8)


if __name__ == '__main__':
    unittest.main()
