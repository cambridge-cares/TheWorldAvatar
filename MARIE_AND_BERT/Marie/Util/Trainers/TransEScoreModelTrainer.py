from pprint import pprint

import numpy as np
import torch
from torch import no_grad
import os
import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import AdamW
from Marie.Util.Dataset.TransEScoreModel_Dataset import Dataset
from Marie.Util.Models.TransERelPredictionModel import TransERelPredictionModel
from Marie.Util.location import DATA_DIR


class TransEScoreModelTrainer:
    def __init__(self, dataset_dir="CrossGraph/ontokin"):
        self.dataset_dir = dataset_dir

    def one_train_iteration(self, learning_rate=1e-8, model_name='bert_model_embedding_20_cosine_self_made',
                            resume_training=False, batch_size=64, epoch_num=100):
        learning_rate = learning_rate
        step = 0
        # ===========================
        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {device} ===============')
        # ===========================

        df = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv'), sep='\t', index_col=0)
        df = df.drop(columns=["head", "tail"])
        df = df.drop_duplicates()
        df = df.reset_index(drop=True)
        df_train = df
        df_test = df
        dataset_test = Dataset(df_test, self.dataset_dir)
        dataset_train = Dataset(df_train, self.dataset_dir)
        dim = dataset_train.dim
        test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        model = TransERelPredictionModel(device=device, dim=dim, dataset_dir=self.dataset_dir)
        optimizer = AdamW(model.parameters(), lr=learning_rate)
        model = model.to(device)
        if resume_training:
            model.load_model(model_name)

        for epoch in range(epoch_num):
            total_loss_train = 0
            model.train()
            for train_batch in tqdm(train_dataloader):
                true_y = train_batch[1].to(device)  # the y is replaced by the embedding now, the loss is the distance
                loss, output = model(train_batch[0], true_y)
                loss.mean().backward()
                optimizer.step()
                step += 1
                total_loss_train += loss.mean().item()

            print(f'\ntotal_loss_train: {total_loss_train}')
            if epoch % 10 == 0:
                with no_grad():
                    model.eval()
                    total_loss_val = 0
                    avg_cos_similarity = 0
                    dist_similarity = 0
                    for test_batch in tqdm(test_dataloader):
                        true_y = test_batch[1].to(
                            device)  # the y is replaced by the embedding now, the loss is the distance
                        loss, output_val = model(test_batch[0], true_y)
                        total_loss_val += loss.detach().mean().item()
                        test_cosine = torch.nn.CosineSimilarity(dim=1, eps=1e-08)
                        cos_similarity = test_cosine(output_val, true_y).detach()
                        avg_cos_similarity += torch.mean(cos_similarity).detach().item()
                        dist_similarity += (output_val - true_y).norm(p=1, dim=1).detach().mean()

                    print(f'\ntotal_loss_val: {total_loss_val}')
                    print('average cosine similarity', avg_cos_similarity / len(test_dataloader))
                    print('average dist similarity', dist_similarity / len(test_dataloader))
        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
        print('model saved')


if __name__ == '__main__':
    # starting_lr = 1e-20  # this is probably the best lr
    starting_lr = 1e-5
    current_lr = starting_lr
    my_trainer = TransEScoreModelTrainer(dataset_dir="CrossGraph/wikidata_numerical")
    my_trainer.one_train_iteration(current_lr,
                                   model_name='bert_ontospecies',
                                   resume_training=False, batch_size=32)
    for i in range(10):
        print(f'current learning rate {current_lr}')
        my_trainer.one_train_iteration(current_lr,
                                       model_name='bert_ontospecies',
                                       resume_training=True, batch_size=32)
        current_lr = current_lr / 10
