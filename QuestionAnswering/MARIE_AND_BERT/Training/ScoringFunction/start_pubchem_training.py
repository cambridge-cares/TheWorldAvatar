# ======================= init dependencies and directory info ===================
import sys

sys.path.append('../..')
import os
import pickle
import pandas as pd

from Marie.Util.location import DATA_DIR
from Training.Embedding.TransE_Trainer import Trainer as TransETrainer
from Marie.Util.Embedding.EmbeddingTrainer import Trainer as GenericTrainer


# STEP 0
# create training set of size x

# STEP 1:
# create entity2idx.pickle and relation2idx.pickle



def train_TransE(dataset_name):
    my_transe_trainer = TransETrainer(dataset_name=dataset_name, load_pretrained_embeddings=False, dim=20,
                                      batch_size=64)
    my_transe_trainer.train()


def train_Complex(dataset_name):
    #  model, dataset_name, epochs=100, learning_rate=0.01,
    #                  data_folder='ontocompchem_calculation', save_model=False, complex=True
    my_complex_trainer = GenericTrainer(dataset_name=dataset_name, save_model=True, dim=30, epochs=100)
    my_complex_trainer.train()


if __name__ == '__main__':
    dataset_name = 'ontocompchem_calculation'
    create_indexing(dataset_name=dataset_name, data_dir='ontocompchem_calculation_latent')
    # rain_TransE(dataset_name=dataset_name)
