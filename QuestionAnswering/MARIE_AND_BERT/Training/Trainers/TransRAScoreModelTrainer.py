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

        self.test_cosine = torch.nn.CosineSimilarity(dim=0, eps=1e-08)

    def evaluate_best_match(self, target_embeddings, predicted_output_list, true_output_embedding_list):
        idx_list = []
        full_embeddings = torch.LongTensor([target_embeddings.values] * len(predicted_output_list))
        for embeddings, predicted_output, true_output_embedding in zip(full_embeddings, predicted_output_list,
                                                                       true_output_embedding_list):
            best_idx = argmax(self.test_cosine(predicted_output, embeddings))
            true_idx = argmax(self.test_cosine(true_output_embedding, embeddings))
            if best_idx == true_idx:
                idx_list.append(1)
            else:
                idx_list.append(0)
        return idx_list

    def one_train_iteration(self, learning_rate=1e-8, model_name='bert_model_embedding_20_cosine_self_made',
                            resume_training=False, batch_size=64, epoch_num=250, gamma=1.0, test_step=1,
                            scheduler_step=100, mode="prediction", dictionary={}):
        learning_rate = learning_rate
        step = 0
        # ===========================
        use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if use_cuda else "cpu")
        print(f'=========== USING {device} ===============')
        # ===========================
        df_path = os.path.join(DATA_DIR, self.dataset_dir, 'score_model_training.tsv')
        print(f'Loading training data from {df_path}')
        df = pd.read_csv(df_path, sep='\t', index_col=0)
        # print(df)
        df = df.drop(columns=["head", "tail"])
        df = df.drop_duplicates()
        df = df.reset_index(drop=True)
        df_train = df
        df_test = df
        dataset_test = Dataset(df_test, self.dataset_dir, operator_dict=dictionary)
        dataset_train = Dataset(df_train, self.dataset_dir, operator_dict=dictionary)
        dim = dataset_train.dim
        test_dataloader = torch.utils.data.DataLoader(dataset_test, batch_size=batch_size, shuffle=True)
        train_dataloader = torch.utils.data.DataLoader(dataset_train, batch_size=batch_size, shuffle=True)
        model = TransEAScoreModel(device=device, dim=dim, dataset_dir=self.dataset_dir, output_dim=len(dictionary))
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
                if mode == "prediction":
                    loss_P.mean().backward()
                    optimizer.step()
                    step += 1
                    total_loss_train_P += loss_P.mean().item()
                    print(f'\ntotal_loss_train_prediction: {total_loss_train_P} - epoch: {epoch}')
                elif mode == "operator":
                    loss_O.mean().backward()
                    optimizer.step()
                    step += 1
                    total_loss_train_O += loss_O.mean().item()
                    print(f'\ntotal_loss_train_operator: {total_loss_train_O} - epoch: {epoch}')

                else:
                    loss = loss_O + loss_P
                    loss.mean().backward()
                    optimizer.step()
                    step += 1
                    total_loss_train_O += loss_O.mean().item()
                    total_loss_train_P += loss_P.mean().item()

                print(f'\ntotal_loss_train_operator: {total_loss_train_O} - epoch: {epoch}')
                print(f'\ntotal_loss_train_prediction: {total_loss_train_P} - epoch: {epoch}')
            if (epoch + 1) % scheduler_step == 0:
                scheduler.step()
                print(f"change learning rate to {scheduler.get_lr()}")

            if (epoch + 1) % test_step == 0:
                with no_grad():
                    model.eval()
                    total_loss_val = 0
                    total_hit_rate = []
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

                        loss, output_val_R, output_val_A, output_val_O, output_val_B, _ = \
                            model(test_batch[0], true_y_R, true_y_A, true_y_O, true_y_B)

                        # get the best match among all embeddings
                        rel_hit_rate = self.evaluate_best_match(target_embeddings=model.rel_embedding,
                                                                true_output_embedding_list=true_y_R.cpu(),
                                                                predicted_output_list=output_val_R.cpu())

                        attr_hit_rate = self.evaluate_best_match(target_embeddings=model.attr_embedding,
                                                                 true_output_embedding_list=true_y_A.cpu(),
                                                                 predicted_output_list=output_val_A.cpu())
                        bias_hit_rate = self.evaluate_best_match(target_embeddings=model.bias_embedding,
                                                                 true_output_embedding_list=true_y_B.cpu(),
                                                                 predicted_output_list=output_val_B.cpu())

                        total_hit_rate += rel_hit_rate
                        total_hit_rate += attr_hit_rate
                        total_hit_rate += bias_hit_rate


                        total_loss_val += loss.detach().mean().item()
                        test_cosine = torch.nn.CosineSimilarity(dim=0, eps=1e-08)
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

                    avg_cos_similarity_R = avg_cos_similarity_R / len(test_dataloader)
                    avg_cos_similarity_A = avg_cos_similarity_A / len(test_dataloader)
                    avg_cos_similarity_B = avg_cos_similarity_B / len(test_dataloader)

                    avg_cos_similarity = [avg_cos_similarity_R, avg_cos_similarity_A, avg_cos_similarity_B]
                    avg_cos_similarity = sum(avg_cos_similarity) / len(avg_cos_similarity)
                    avg_hit_rate = sum(total_hit_rate) / len(total_hit_rate)
                    print(f'\ntotal_loss_val: {total_loss_val}')
                    print('average cosine similarity_R', avg_cos_similarity_R)
                    print('average cosine similarity_A', avg_cos_similarity_A)
                    print('average cosine similarity_B', avg_cos_similarity_B)
                    print("===================================================")
                    print("avg_cos_similarity", avg_cos_similarity)
                    print("avg_hit_rate", avg_hit_rate)
                    print("===================================================")
                    if avg_hit_rate >= 0.99:
                        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
                        print('model saved')
                        return None

                    # print('total_operator_hit_rate', total_operator_hit_rate / len(test_dataloader))
                    if total_operator_hit_rate / len(test_dataloader) == 1:
                        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
                        print('model saved')
                        return None

        torch.save(model.state_dict(), os.path.join(DATA_DIR, self.dataset_dir, model_name))
        print('model saved')


if __name__ == '__main__':
    if __name__ == '__main__':
        import argparse

        parser = argparse.ArgumentParser()
        parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
        parser.add_argument("-g", "--gamma", help="gamma for scheduler")
        parser.add_argument("-o", "--ontology", help="main ontology used")
        parser.add_argument("-bs", "--batch_size", help="size of mini batch")
        parser.add_argument("-resume", "--resume", help="resume the training by loading embeddings ")
        parser.add_argument("-m", "--mode", help="to train a relation prediction model or an operator prediction model")
        parser.add_argument("-so", "--sub_ontology", help="name of the sub ontology")
        parser.add_argument("-epoch", "--epoch", help="number of epochs")
        parser.add_argument("-dict", "--dictionary", help="the encoding dictionary for operators")

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

        mode = "prediction"
        if args.mode:
            mode = args.mode

        dictionary = {0: "smaller", 1: "larger", 2: "none"}
        if args.dictionary:
            # convert the "-" seperated operator to the dictionary
            dictionary = args.dictionary.split("-")
            dictionary = {dictionary[i]: i for i in range(len(dictionary))}
            print(dictionary)

        if sub_ontology:
            full_dir = os.path.join(DATA_DIR, 'CrossGraph', f'{ontology}/{sub_ontology}')
            ontology = sub_ontology
        else:
            full_dir = os.path.join(DATA_DIR, 'CrossGraph', f'{ontology}')

        model_name = f'bert_{ontology}_{mode}'
        if mode == "joint":
            model_name = f'bert_{ontology}'

        my_trainer = TransEAScoreModelTrainer(dataset_dir=full_dir)
        my_trainer.one_train_iteration(learning_rate=learning_rate,
                                       model_name=model_name,
                                       resume_training=resume, batch_size=batch_size, epoch_num=epoch, test_step=1,
                                       scheduler_step=20, gamma=gamma, mode=mode, dictionary=dictionary)
