from pprint import pprint
import sys
sys.path.append("../..")
import numpy as np
import torch
from torch import no_grad
import os
import pandas as pd
from torch.optim.lr_scheduler import ExponentialLR
from torch.utils.data.dataset import Dataset as TorchDataset
from tqdm import tqdm
from transformers import AdamW
from Marie.Util.Dataset.TransEScoreModel_Dataset import Dataset
from Marie.Util.Models.TransERelPredictionModel import TransERelPredictionModel
from Marie.Util.location import DATA_DIR


class TransEScoreModelTrainer:
    def __init__(self, dataset_dir="CrossGraph/ontokin", mode="general", test_step=10):
        self.dataset_dir = dataset_dir
        self.mode = mode
        self.test_step = test_step

    def one_train_iteration(self, learning_rate=1e-8, model_name='bert_model_embedding_20_cosine_self_made',
                            resume_training=False, batch_size=64, epoch_num=100, gamma=1.0):
        learning_rate = learning_rate
        step = 0
        # ===========================
        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {device} ===============')
        # ===========================

        df = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv'), sep='\t',
                         index_col=0)
        print(df)
        self.gamma = gamma
        if self.mode == "agent":
            df = df.drop_duplicates()
            df = df.reset_index(drop=True)
        else:
            # df = df.drop(columns=["head", "tail"])
            # df = df.drop_duplicates()
            df = df.reset_index(drop=True)

        df_train = df
        df_test = df
        dataset_test = Dataset(df_test, self.dataset_dir, mode=self.mode)
        dataset_train = Dataset(df_train, self.dataset_dir, mode=self.mode)
        if self.mode == "agent":
            self.ent_embedding_dict = dataset_test.ent_embedding_dict
            self.idx2entity_dict = dataset_test.idx2entity_dict

        dim = dataset_train.dim
        test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        model = TransERelPredictionModel(device=device, dim=dim, dataset_dir=self.dataset_dir, mode=self.mode)
        self.optimizer = AdamW(model.parameters(), lr=learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

        model = model.to(device)
        test_cosine = torch.nn.CosineSimilarity(dim=1, eps=1e-08)

        if resume_training:
            model.load_model(model_name)

        for epoch in range(epoch_num):
            total_loss_train = 0
            model.train()
            for train_batch in tqdm(train_dataloader):
                if self.mode == "agent":
                    tokenized_question, agent_emb, output_emb, _, _, _ = train_batch
                    # distance_agent, distance_output, normalize(linear_output), normalize(linear_agent)
                    loss, _, _, _, = model(tokenized_question, output_emb.to(device))
                    loss.mean().backward()

                else:
                    true_y = train_batch[1].to(
                        device)  # the y is replaced by the embedd11ing now, the loss is the distance
                    loss, output = model(train_batch[0], true_y)
                    loss.mean().backward()

                total_loss_train += loss.mean().item()
                self.optimizer.step()
                step += 1

            print(f'\ntotal_loss_train: {total_loss_train}')
            if (epoch + 1) % self.test_step == 0:
                with no_grad():
                    model.eval()
                    total_loss_val = 0
                    avg_cos_similarity = 0
                    dist_similarity = 0
                    cos_similarity_output = 0
                    hit_accuracy = 0
                    hit_counter = 0
                    cos_similarity_agent = 0
                    for test_batch in tqdm(test_dataloader):
                        if self.mode == "agent":
                            tokenized_question, true_agent_emb, true_output_emb, agent_name_batch, true_p_idx_batch, all_question = test_batch

                            # distance_agent + distance_output, normalize(linear_output), normalize(linear_agent)
                            linear_output_batch = model.predict(tokenized_question).to(device)

                            for linear_output, true_p_idx, agent_name, question in \
                                    zip(linear_output_batch, true_p_idx_batch, agent_name_batch, all_question):
                                if agent_name == "none":
                                    all_agent_embeddings = [[1.0] * 40]
                                else:
                                    all_agent_embeddings = list(self.ent_embedding_dict[agent_name].values)
                                the_other_agent_name_list = list(set(self.ent_embedding_dict.keys()))
                                if agent_name != "none":
                                    the_other_agent_name_list.remove(agent_name)
                                the_other_agent_name = the_other_agent_name_list[0]
                                for the_other_agent_name in the_other_agent_name_list:
                                    all_agent_embeddings += list(self.ent_embedding_dict[the_other_agent_name].values)
                                all_same_agent_embeddings = torch.FloatTensor(all_agent_embeddings).to(device)
                                # all_same_agent_embeddings = torch.FloatTensor(self.ent_embedding_dict[agent_name].values).to(device)

                                batch_comparison = test_cosine(linear_output.detach(),
                                                               all_same_agent_embeddings.detach())
                                best_pred_idx = torch.argmax(batch_comparison).to(device).item()
                                if agent_name != "none":
                                    idx2entity = self.idx2entity_dict[agent_name]
                                else:
                                    idx2entity = {0: "random question"}
                                is_best_pred_idx = (best_pred_idx == true_p_idx).int()
                                true_p_idx = true_p_idx.item()
                                if is_best_pred_idx == 0:
                                    try:
                                        print("true idx: ", idx2entity[true_p_idx])
                                        print("pred idx: ", idx2entity[best_pred_idx])
                                    except KeyError:
                                        best_pred_idx = best_pred_idx - len(idx2entity)
                                        idx2entity_other = self.idx2entity_dict[the_other_agent_name]
                                        print("agent name: ", agent_name)
                                        print("true idx: ", idx2entity[true_p_idx])
                                        if agent_name != "none":
                                            print("pred idx: ", idx2entity_other[best_pred_idx])
                                        else:
                                            print("pred idx: ", " some other relation that is not none")
                                    print("question: ", question)
                                    print("-----------------------------------------")
                                # if agent_name != "none":
                                hit_accuracy += is_best_pred_idx
                                hit_counter += 1
                            cos_similarity_output += test_cosine(linear_output_batch,
                                                                 true_output_emb.to(device)).detach().mean()
                            # cos_similarity_agent += test_cosine(linear_output,
                            #                                     true_output_emb.to(device)).detach().mean()
                        else:
                            true_y = test_batch[1].to(device)
                            loss, output_val = model(test_batch[0], true_y)
                            cos_similarity = test_cosine(output_val, true_y).detach()
                            avg_cos_similarity += torch.mean(cos_similarity).detach().item()
                            dist_similarity += (output_val - true_y).norm(p=1, dim=1).detach().mean()
                        # total_loss_val += loss.detach().mean().item()
                    # print(f'\ntotal_loss_val: {total_loss_val}')

                    if self.mode == "agent":
                        average_cos_similarity_output = cos_similarity_output / len(test_dataloader)
                        print('average cos_similarity_output', average_cos_similarity_output)
                        print('average best match hit accuracy', hit_accuracy / hit_counter)
                        # if (cos_similarity_output / len(test_dataloader)).item() > 0.99:
                        if (hit_accuracy / hit_counter) == 1:  # and (average_cos_similarity_output > 0.6):
                            torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
                            print('model saved')
                            return None

                        # print('average cos_similarity_agent', cos_similarity_agent / len(test_dataloader))

                    else:
                        print('average cosine similarity', avg_cos_similarity / len(test_dataloader))
                    # print('average dist similarity', dist_similarity / len(test_dataloader))
                self.scheduler.step()
        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
        print('model saved')


if __name__ == '__main__':

    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
    parser.add_argument("-g", "--gamma", help="gamma for scheduler")
    parser.add_argument("-o", "--ontology", help="main ontology used")
    parser.add_argument("-bs", "--batch_size", help="size of mini batch")
    parser.add_argument("-resume", "--resume", help="resume the training by loading embeddings ")
    parser.add_argument("-m", "--mode", help="general training or agent training ")
    parser.add_argument("-so", "--sub_ontology", help="name of the sub ontology")
    parser.add_argument("-epoch", "--epoch", help="number of epochs")

    args = parser.parse_args()

    learning_rate = 1e-5
    if args.learning_rate:
        learning_rate = float(args.learning_rate)

    gamma = 1
    if args.gamma:
        gamma = float(args.gamma)

    batch_size = 32
    if args.batch_size:
        batch_size = int(args.batch_size)

    epoch = 100
    if args.epoch:
        epoch = int(args.epoch)

    ontology = "pubchem"
    if args.ontology:
        ontology = args.ontology

    mode = "general"
    if args.mode:
        mode = args.mode

    sub_ontology = None
    if args.sub_ontology:
        sub_ontology = args.sub_ontology

    resume = False
    if args.resume:
        if args.resume.lower() == "yes":
            resume = True
        elif args.resume.lower() == "no":
            resume = False
        else:
            resume = False
    # starting_lr = 1e-20  # this is probably the best lr
    my_trainer = TransEScoreModelTrainer(dataset_dir=f"CrossGraph/{ontology}", mode=mode, test_step=5)
    my_trainer.one_train_iteration(learning_rate=learning_rate,
                                   model_name=f'bert_{ontology}',
                                   resume_training=resume, batch_size=batch_size, epoch_num=epoch)

