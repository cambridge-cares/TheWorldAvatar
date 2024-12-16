from pprint import pprint
import sys

from numpy import argmax
from torch.optim.lr_scheduler import ExponentialLR

sys.path.append("../..")
import numpy as np
import torch
from torch import no_grad
import os
import pandas as pd
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import AdamW
from Marie.Util.Dataset.TransEAScoreModel_Dataset import Dataset
from Marie.Util.Models.TransEAScoreModel import TransEAScoreModel
from Marie.Util.location import DATA_DIR


def measure_hit_binary(predicted_y, true_y):
    # TODO: do it the numpy
    predicted_y = predicted_y.cpu()
    predicted_y[predicted_y > 0.5] = 1
    predicted_y[predicted_y <= 0.5] = 0
    true_y = true_y.cpu()
    rate = (predicted_y == true_y).all(dim=1).float().mean()
    return rate


class TransEAScoreModelTrainer:
    def __init__(self, dataset_dir="CrossGraph/ontokin"):
        self.dataset_dir = dataset_dir

    def one_train_iteration(self, learning_rate=1e-8, model_name='bert_model_embedding_20_cosine_self_made',
                            resume_training=False, batch_size=64, epoch_num=250, gamma=1.0, test_step=1,
                            scheduler_step=100):
        learning_rate = learning_rate
        step = 0
        # ===========================
        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {device} ===============')
        # ===========================

        df = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv'), sep='\t', index_col=0)
        print(df)
        df = df.drop(columns=["head", "tail"])
        # df = df.drop_duplicates()
        df = df.reset_index(drop=True)
        df_train = df
        df_test = df
        dataset_test = Dataset(df_test, self.dataset_dir)
        dataset_train = Dataset(df_train, self.dataset_dir)
        dim = dataset_train.dim
        test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        model = TransEAScoreModel(device=device, dim=dim, dataset_dir=self.dataset_dir)
        optimizer = AdamW(model.parameters(), lr=learning_rate)
        scheduler = ExponentialLR(optimizer, gamma=gamma)

        model = model.to(device)
        if resume_training:
            model.load_model(model_name)
        #
        for epoch in range(epoch_num):
            total_loss_train_P = 0
            total_loss_train_O = 0

            model.train()
            for train_batch in tqdm(train_dataloader):
                true_y_R = train_batch[1].to(device)
                true_y_A = train_batch[2].to(device)
                true_y_O = train_batch[3].to(device)
                true_y_B = train_batch[4].to(device)
                loss_O, _, _, _, _, loss_P = model(train_batch[0], true_y_R, true_y_A, true_y_O, true_y_B)
                # loss = loss_P# loss_O + loss_P
                # loss = loss_O
                # loss.mean().backward()
                loss_O.mean().backward()
                optimizer.step()
                step += 1
                # loss, _, _, _, _, loss_P = model(train_batch[0], true_y_R, true_y_A, true_y_O, true_y_B)
                # loss_P.mean().backward()
                # optimizer.step()
                # step += 1

                total_loss_train_O += loss_O.mean().item()
                # total_loss_train_P += loss_P.mean().item()

            print(f'\ntotal_loss_train_operator: {total_loss_train_O} - epoch: {epoch}')
           #  print(f'\ntotal_loss_train_prediction: {total_loss_train_P} - epoch: {epoch}')
            if (epoch + 1) % scheduler_step == 0:
                scheduler.step()
                print(f"change learning rate to {scheduler.get_lr()}")

            if (epoch + 1) % test_step == 0:
                with no_grad():
                    model.eval()
                    total_loss_val = 0

                    total_operator_hit_rate = 0
                    avg_cos_similarity_R = 0
                    dist_similarity_R = 0
                    avg_cos_similarity_A = 0
                    dist_similarity_A = 0
                    avg_cos_similarity_B = 0
                    dist_similarity_B = 0

                    for test_batch in tqdm(test_dataloader):
                        true_y_R = test_batch[1].to(device)
                        true_y_A = test_batch[2].to(device)
                        true_y_O = test_batch[3].to(device)
                        true_y_B = test_batch[4].to(device)

                        loss, output_val_R, output_val_A, output_val_O, output_val_B, _= \
                            model(test_batch[0], true_y_R, true_y_A, true_y_O, true_y_B)
                        total_loss_val += loss.detach().mean().item()
                        test_cosine = torch.nn.CosineSimilarity(dim=1, eps=1e-08)
                        # ======================= test for R accuracy =====================
                        cos_similarity_R = test_cosine(output_val_R, true_y_R).detach()
                        avg_cos_similarity_R += torch.mean(cos_similarity_R).detach().item()
                        dist_similarity_R += (output_val_R - true_y_R).norm(p=1, dim=1).detach().mean()
                        # ======================= test for A accuracy =====================
                        cos_similarity_A = test_cosine(output_val_A, true_y_A).detach()
                        avg_cos_similarity_A += torch.mean(cos_similarity_A).detach().item()
                        dist_similarity_A += (output_val_A - true_y_A).norm(p=1, dim=1).detach().mean()
                        # ======================= test for O accuracy =====================
                        operator_hit_rate = measure_hit_binary(predicted_y=output_val_O, true_y=true_y_O)
                        total_operator_hit_rate += operator_hit_rate
                        # ======================= test for B accuracy =====================
                        cos_similarity_B = test_cosine(output_val_B, true_y_B).detach()
                        avg_cos_similarity_B += torch.mean(cos_similarity_B).detach().item()
                        dist_similarity_B += (output_val_B - true_y_B).norm(p=1, dim=1).detach().mean()

                    print(f'\ntotal_loss_val: {total_loss_val}')

                    print('average cosine similarity_R', avg_cos_similarity_R / len(test_dataloader))
                    print('average dist similarity_R', dist_similarity_R / len(test_dataloader))
                    print('average cosine similarity_A', avg_cos_similarity_A / len(test_dataloader))
                    print('average dist similarity_A', dist_similarity_A / len(test_dataloader))
                    print('average cosine similarity_B', avg_cos_similarity_B / len(test_dataloader))
                    print('average dist similarity_B', dist_similarity_B / len(test_dataloader))
                    print('total_operator_hit_rate', total_operator_hit_rate / len(test_dataloader))

        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
        print('model saved')


if __name__ == '__main__':
    # starting_lr = 1e-20  # this is probably the best lr
    starting_lr = 1e-5
    current_lr = starting_lr
    ontology = "OntoMoPs"
    sub_ontology = "numerical_with_implicit"
    my_trainer = TransEAScoreModelTrainer(dataset_dir=f"CrossGraph/{ontology}/{sub_ontology}")
    my_trainer.one_train_iteration(current_lr,
                                   model_name=f'bert_{ontology}_operator',
                                   resume_training=True, batch_size=32, epoch_num=20, test_step=0,
                                   scheduler_step=20, gamma=1)

    # BERT_MoPs -> 1e-5, step: e-2

    # for i in range(5):
    #     print(f'current learning rate {current_lr}')
    #     my_trainer.one_train_iteration(current_lr,
    #                                    model_name=f'bert_{ontology}_test', epoch_num=40, test_step=20,
    #                                    resume_training=True, batch_size=8)
    #     current_lr = current_lr / 10
