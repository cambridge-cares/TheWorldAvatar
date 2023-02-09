import random
import sys

from torch import no_grad
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

sys.path.append("../../..")

import json
import os

import pandas as pd
import torch

from Marie.Util.Models.TransR import TransR
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.Dataset.TransR_Inference_Dataset import TransRInferenceDataset
from Marie.Util.location import DATA_DIR
from Marie.Util.NHopExtractor import HopExtractor


def hit_rate(true_tail_idx_ranking_list):
    hit_1 = 0
    hit_5 = 0
    hit_10 = 0
    counter = 0
    for true_tail_idx_ranking in true_tail_idx_ranking_list:
        if true_tail_idx_ranking == 0:
            hit_1 += 1
            hit_5 += 1
            hit_10 += 1
        elif 0 < true_tail_idx_ranking <= 4:
            hit_5 += 1
            hit_10 += 1
        elif 4 < true_tail_idx_ranking <= 9:
            hit_10 += 1
        counter += 1
    hit_1 = hit_1 / counter
    hit_5 = hit_5 / counter
    hit_10 = hit_10 / counter
    return hit_1, hit_5, hit_10


class InferenceTrainer:

    def __init__(self, full_dataset_dir, ontology, batch_size=32, epoch_num=100, dim=20, learning_rate=1.0, gamma=1,
                 test=False, use_projection=False, alpha = 0.1):
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.dim = dim
        self.test = test
        self.use_projection = use_projection
        self.alpha = alpha

        self.my_extractor = HopExtractor(
            dataset_dir=self.full_dataset_dir,
            dataset_name=self.ontology)
        df_train = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-train-2.txt"), sep="\t", header=None)
        df_train_small = df_train.sample(frac=0.01)
        df_test = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-test.txt"), sep="\t", header=None)
        df_numerical = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-numerical.txt"), sep="\t", header=None)

        train_numerical_set = TransRInferenceDataset(df_numerical, full_dataset_dir=full_dir, ontology=self.ontology,
                                                     mode="numerical")

        test_set = TransRInferenceDataset(df_test, full_dataset_dir=self.full_dataset_dir, ontology=self.ontology,
                                          mode="test")

        # ========================================== CREATE DATASET FOR TRAINING ================================
        if not self.test:
            train_set = TransRInferenceDataset(df_train, full_dataset_dir=self.full_dataset_dir, ontology=self.ontology,
                                               mode="train")
            self.train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size, shuffle=True)
        # =========================================================================================================

        train_set_small = TransRInferenceDataset(df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                 ontology=self.ontology,
                                                 mode="train")

        train_set_eval = TransRInferenceDataset(df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                ontology=self.ontology,
                                                mode="train_eval")
        self.use_cuda = torch.cuda.is_available()

        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        print(f"==================== USING {self.device} =====================")
        # ================================================================================================================
        self.test_dataloader = torch.utils.data.DataLoader(test_set, batch_size=test_set.candidate_max, shuffle=False)
        self.train_numerical_dataloader = torch.utils.data.DataLoader(train_numerical_set, batch_size=32,
                                                                      shuffle=True)

        self.train_dataloader_small = torch.utils.data.DataLoader(train_set_small, batch_size=batch_size, shuffle=True)
        self.train_dataloader_eval = torch.utils.data.DataLoader(train_set_eval, batch_size=train_set_eval.ent_num,
                                                                 shuffle=False)

        # ===========================================================================================================
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()

        # ------------------------- Training hyperparameters -----------------------
        self.epoch_num = epoch_num
        self.model = TransR(rel_dim=self.dim, rel_num=len(self.rel2idx.keys()), ent_dim=self.dim,
                            ent_num=len(self.entity2idx.keys()), device=self.device,
                            use_projection=self.use_projection, alpha = self.alpha).to(self.device)
        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

    def export_embeddings(self):
        self.write_embeddings(self.model.ent_embedding, "ent_embedding")
        self.write_embeddings(self.model.rel_embedding, "rel_embedding")
        # self.write_embeddings(self.model.attr_embedding, "attr_embedding")
        # self.write_embeddings(self.model.bias, "bias_embedding")

    def write_embeddings(self, embedding, embedding_name):
        lines = []
        for embedding in embedding.weight.data:
            line = '\t'.join([str(l) for l in embedding.tolist()])
            lines.append(line)
        content = '\n'.join(lines)
        with open(os.path.join(DATA_DIR, self.full_dataset_dir, f'{embedding_name}.tsv'), 'w') as f:
            f.write(content)
            f.close()

    def evaluate(self):
        with no_grad():
            self.model.eval()
            total_mrr = 0
            counter = 0
            filtered_hit_rate_list = []
            for test_set in tqdm(self.train_dataloader_eval):
                heads, rels, all_tails, true_tail = test_set[0], test_set[1], test_set[2], test_set[3][0].item()
                selected_idx = (all_tails >= 0)
                heads, rels, all_tails = heads[selected_idx], rels[selected_idx], all_tails[selected_idx]
                triples = torch.stack((heads, rels, all_tails)).type(torch.LongTensor)
                distances = self.model.predict(triples=triples)
                _, B = torch.topk(distances, k=len(distances), largest=False)
                B = B.tolist()
                selected_candidates = [all_tails.tolist()[idx] for idx in B]
                if true_tail in selected_candidates:
                    ranking_idx = selected_candidates.index(true_tail)
                    ranking = 1 / (ranking_idx + 1)
                else:
                    ranking = 0
                    ranking_idx = -1

                filtered_hit_rate_list.append(ranking_idx)
                counter += 1
                total_mrr += ranking
            total_mrr = total_mrr / counter
            filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
            print("=================== Training set evaluation ===================")
            print(f"total train mrr: {total_mrr}")
            print(f"the training hit rate list is : {filtered_hit_rate_list}")
            print("===============================================================")

            total_mrr, counter, filtered_counter, total_fmrr = 0, 0, 0, 0
            hit_rate_list = []
            filtered_hit_rate_list = []
            for test_set in tqdm(self.test_dataloader):
                heads, rels, all_tails, true_tail = test_set[0], test_set[1], test_set[2], test_set[3][0].item()
                selected_idx = (all_tails >= 0)
                heads, rels, all_tails = heads[selected_idx], rels[selected_idx], all_tails[selected_idx]
                triples = torch.stack((heads, rels, all_tails)).type(torch.LongTensor)
                distances = self.model.infer(triples)
                _, B = torch.topk(distances, k=len(distances), largest=False)
                B = B.tolist()
                selected_candidates = [all_tails.tolist()[idx] for idx in B]

                if true_tail in selected_candidates:
                    f_ranking_idx = selected_candidates.index(true_tail)
                    f_ranking = 1 / (f_ranking_idx + 1)
                else:
                    f_ranking = 0
                    f_ranking_idx = -1
                filtered_counter += 1
                counter += 1
                total_fmrr += f_ranking
                hit_rate_list.append(ranking_idx)
                filtered_hit_rate_list.append(f_ranking_idx)

            filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
            total_fmrr = total_fmrr / filtered_counter
            print("=================== Inference evaluation result ====================")
            print(f"total infer fmrr: {total_fmrr}")
            print(f"filtered infer hit rate: {filtered_hit_rate_list}")
            print("====================================================================")

    def train(self):
        """
        Split the the training set into non-numerical and numerical subsets
        marked by [3] == -999 or not
        :return:
        """
        self.model.train()
        total_train_loss = 0
        if self.test:
            self.train_dataloader = self.train_dataloader_small
            # in test mode, use self.train_dataloader_small

        for pos, neg in tqdm(self.train_dataloader):
            self.optimizer.zero_grad()
            numerical_idx_list = (pos[3] != -999)
            pos = torch.transpose(torch.stack(pos), 0, 1)
            pos_numerical = torch.transpose(pos[numerical_idx_list], 0, 1)
            pos_non_numerical = torch.transpose(pos[~numerical_idx_list], 0, 1)  # create negative index list with ~
            neg = torch.transpose(torch.stack(neg), 0, 1)
            neg_numerical = torch.transpose(neg[numerical_idx_list], 0, 1)
            neg_non_numerical = torch.transpose(neg[~numerical_idx_list], 0, 1)
            loss_non_numerical = self.model(pos_non_numerical, neg_non_numerical, mode="non_numerical")
            # print("loss_non_numerical", loss_non_numerical)
            # loss_non_numerical.backward()
            if len(pos_numerical[0]) > 0:
                loss_numerical = self.model(pos_numerical, neg_numerical, mode="numerical")
                # print("loss_numerical", loss_numerical)
                loss = loss_numerical.mean() + loss_non_numerical.mean()
                loss.backward()
            else:
                loss = loss_non_numerical.mean()
                loss.backward()

            total_train_loss += loss.cpu().mean()
            self.optimizer.step()
            self.model.normalize_parameters()

        print(f"Loss: {total_train_loss}")

    def run(self):
        for epoch in range(self.epoch_num + 1):
            print(f"Epoch: {epoch}")
            self.train()
            if epoch % 10 == 0:
                self.scheduler.step()
                self.evaluate()
                self.export_embeddings()
                print(f"Current learning rate: {self.scheduler.get_lr()}")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dimension", help="dimension of embedding")
    parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
    parser.add_argument("-g", "--gamma", help="gamma for scheduler")
    parser.add_argument("-so", "--sub_ontology", help="name of the sub ontology")
    parser.add_argument("-bs", "--batch_size", help="size of mini batch")
    parser.add_argument("-test", "--test_mode", help="if true, the training will use a smaller training set")
    parser.add_argument("-proj", "--use_projection", help="if true, use projection in numerical linear regression")
    parser.add_argument("-alpha", "--alpha", help="ratio between l_a and l_r")
    args = parser.parse_args()

    dim = 20
    if args.dimension:
        dim = int(args.dimension)

    learning_rate = 0.01
    if args.learning_rate:
        learning_rate = float(args.learning_rate)

    alpha = 0.1
    if args.alpha:
        alpha = float(args.alpha)

    gamma = 1
    if args.gamma:
        gamma = float(args.gamma)

    batch_size = 256
    if args.batch_size:
        batch_size = int(args.batch_size)

    ontology = "role_with_subclass_mass"
    if args.sub_ontology:
        ontology = args.sub_ontology

    test = False
    if args.test_mode:
        if args.test_mode.lower() == "yes":
            test = True
        elif args.test_mode.lower() == "no":
            test = False
        else:
            test = False

    use_projection = False
    if args.use_projection:
        if args.use_projection.lower() == "yes":
            use_projection = True
        elif args.use_projection.lower() == "no":
            use_projection = False
        else:
            use_projection = False

    print(f"Dimension: {dim}")
    print(f"Learning rate: {learning_rate}")
    print(f"Gamma: {gamma}")
    print(f"Test: {test}")
    print(f"Batch size: {batch_size}")
    print(f"Alpha: {alpha}")
    print(f"Use projection: {use_projection}")
    print(f"Test: {test}")

    full_dir = os.path.join(DATA_DIR, 'CrossGraph', 'ontospecies_new/role_with_subclass_mass')
    my_trainer = InferenceTrainer(full_dataset_dir=full_dir, ontology=ontology, batch_size=batch_size, dim=dim,
                                  learning_rate=learning_rate, test=test, use_projection=use_projection, alpha=alpha)
    my_trainer.run()
