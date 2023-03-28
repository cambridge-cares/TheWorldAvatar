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
    def __init__(self, dataset_dir="CrossGraph/ontokin", mode="general", test_step = 10):
        self.dataset_dir = dataset_dir
        self.mode = mode
        self.test_step = test_step

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

        if self.mode == "agent":
            pass
        else:
            df = df.drop(columns=["head", "tail"])
            df = df.drop_duplicates()
            df = df.reset_index(drop=True)

        df_train = df
        df_test = df
        dataset_test = Dataset(df_test, self.dataset_dir, mode=self.mode)
        dataset_train = Dataset(df_train, self.dataset_dir, mode=self.mode)
        dim = dataset_train.dim
        test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        model = TransERelPredictionModel(device=device, dim=dim, dataset_dir=self.dataset_dir, mode=self.mode)
        optimizer = AdamW(model.parameters(), lr=learning_rate)
        model = model.to(device)
        test_cosine = torch.nn.CosineSimilarity(dim=1, eps=1e-08)

        if resume_training:
            model.load_model(model_name)

        for epoch in range(epoch_num):
            total_loss_train = 0
            model.train()
            for train_batch in tqdm(train_dataloader):
                if self.mode == "agent":
                    tokenized_question, agent_emb, output_emb = train_batch
                    # distance_agent, distance_output, normalize(linear_output), normalize(linear_agent)
                    loss, _, _ = model(tokenized_question, output_emb.to(device),
                                       true_linear_agent=agent_emb.to(device))
                    loss.mean().backward()

                else:
                    true_y = train_batch[1].to(
                        device)  # the y is replaced by the embedd11ing now, the loss is the distance
                    loss, output = model(train_batch[0], true_y)
                    loss.mean().backward()

                total_loss_train += loss.mean().item()
                optimizer.step()
                step += 1

            print(f'\ntotal_loss_train: {total_loss_train}')
            if epoch % self.test_step == 0:
                with no_grad():
                    model.eval()
                    total_loss_val = 0
                    avg_cos_similarity = 0
                    dist_similarity = 0
                    cos_similarity_output = 0
                    cos_similarity_agent = 0
                    for test_batch in tqdm(test_dataloader):
                        if self.mode == "agent":
                            tokenized_question, true_agent_emb, true_output_emb = test_batch
                            # distance_agent + distance_output, normalize(linear_output), normalize(linear_agent)
                            loss, linear_output, linear_agent = model(tokenized_question, true_agent_emb.to(device),
                                                                      true_linear_agent=true_output_emb.to(device))

                            cos_similarity_output += test_cosine(linear_agent, true_agent_emb.to(device)).detach().mean()
                            cos_similarity_agent += test_cosine(linear_output, true_output_emb.to(device)).detach().mean()
                        else:
                            true_y = test_batch[1].to(device)
                            loss, output_val = model(test_batch[0], true_y)
                            cos_similarity = test_cosine(output_val, true_y).detach()
                            avg_cos_similarity += torch.mean(cos_similarity).detach().item()
                            dist_similarity += (output_val - true_y).norm(p=1, dim=1).detach().mean()
                        total_loss_val += loss.detach().mean().item()
                    print(f'\ntotal_loss_val: {total_loss_val}')

                    if self.mode == "agent":
                        print('average cos_similarity_output', cos_similarity_output / len(test_dataloader))
                        print('average cos_similarity_agent', cos_similarity_agent / len(test_dataloader))

                    else:
                        print('average cosine similarity', avg_cos_similarity / len(test_dataloader))
                    # print('average dist similarity', dist_similarity / len(test_dataloader))

        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
        print('model saved')


if __name__ == '__main__':
    # starting_lr = 1e-20  # this is probably the best lr
    starting_lr = 1e-5
    batch_size = 32
    current_lr = starting_lr
    my_trainer = TransEScoreModelTrainer(dataset_dir="CrossGraph/agents", mode="agent", test_step = 5)
    my_trainer.one_train_iteration(current_lr,
                                   model_name='bert_ontoagent',
                                   resume_training=False, batch_size=batch_size, epoch_num=5)
    for i in range(10):
        print(f'current learning rate {current_lr}')
        my_trainer.one_train_iteration(current_lr,
                                       model_name='bert_ontoagent',
                                       resume_training=True, batch_size=batch_size, epoch_num=5)
        current_lr = current_lr / 10
