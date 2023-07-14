import random
import sys

from torch import no_grad, nn
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

import os, sys
sys.path.append('')
sys.path.append('../../Marie/Util/')
sys.path.append('../../Marie/')
sys.path.append("../..")

import os

import pandas as pd
import torch

from Marie.Util.Models.TransEA import TransEA
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


def evaluate_ranking(distances, all_tails, true_tail):
    _, B = torch.topk(distances, k=len(distances), largest=False)
    B = B.tolist()
    selected_candidates = [all_tails.tolist()[idx] for idx in B]
    if true_tail in selected_candidates:
        f_ranking_idx = selected_candidates.index(true_tail)
        f_ranking = 1 / (f_ranking_idx + 1)
    else:
        f_ranking = 0
        f_ranking_idx = -1
    return f_ranking, f_ranking_idx


class InferenceTrainer:

    def __init__(self, full_dataset_dir, ontology, batch_size=32, epoch_num=100, dim=20, learning_rate=1.0, gamma=1,
                 test=False, use_projection=False, alpha=0.1, margin=5, resume=False, inference=True, global_neg=False,
                 gpu_number=1):
        self.full_dataset_dir = full_dataset_dir
        self.ontology = ontology
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.dim = dim
        self.gpu_number = gpu_number
        self.batch_size = batch_size
        self.test = test
        self.use_projection = use_projection
        self.alpha = alpha
        self.margin = margin
        self.resume = resume
        self.global_neg = global_neg
        self.inference = inference
        self.my_extractor = HopExtractor(
            dataset_dir=self.full_dataset_dir,
            dataset_name=self.ontology)
        df_train = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-train-2.txt"), sep="\t", header=None)
        self.df_train = df_train
        df_train_small = df_train.sample(frac=0.01)
        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        numerical_eval_path = os.path.join(full_dir, f"numerical_eval.tsv")

        # ============================== TODO: clean up the training data loading mechanism ============================
        # ========================================== CREATE DATASET FOR TRAINING ================================
        # 1. if test is used, use the df_train_small instead of the full dataset
        # 2. Inference flag should only effect whether the evaluation uses inference
        # =======================================================================================================

        # ================================== Load test set for inference =======================================
        if self.inference:
            df_test = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-test.txt"), sep="\t", header=None)
            test_set = TransRInferenceDataset(df_test, full_dataset_dir=self.full_dataset_dir,
                                              ontology=self.ontology,
                                              mode="test")
            self.test_dataloader = torch.utils.data.DataLoader(test_set,
                                                               batch_size=test_set.candidate_max * self.gpu_number,
                                                               shuffle=False)
        # ================================== Load training set for general embedding ============================
        if self.test:
            print("Using small dataset for testing")
            df_train = df_train_small

        train_set = TransRInferenceDataset(df_train, full_dataset_dir=self.full_dataset_dir,
                                           ontology=self.ontology,
                                           mode="general_train", global_neg=False)
        self.train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size, shuffle=True)

        # ==================================== Load evaluation set, which is a smaller training set ===============
        train_set_eval = TransRInferenceDataset(df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                ontology=self.ontology,
                                                mode="general_train_eval")
        self.train_dataloader_eval = torch.utils.data.DataLoader(train_set_eval,
                                                                 batch_size=train_set_eval.ent_num * self.gpu_number,
                                                                 shuffle=False)
        # ======================== Load training and evaluation set for singular nodes ==============================
        # check whether singular-train.tsv exist
        value_node_path = os.path.join(full_dir, f"{self.ontology}-singular-train.txt")
        if os.path.exists(value_node_path):
            print("Singular node training set exists", value_node_path)
            # df_value_node = pd.read_csv(value_node_path, sep="\t", header=None)
            df_value_node = self.df_train
            self.value_node_exists = True
        else:
            print("Singular node trianing set not exists", value_node_path)
            df_value_node = self.df_train
            self.value_node_exists = False

        if self.value_node_exists:
            value_node_eval_set = TransRInferenceDataset(df=df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                         ontology=self.ontology,
                                                         mode="value_node_eval")
            value_node_set = TransRInferenceDataset(df=df_value_node, full_dataset_dir=self.full_dataset_dir,
                                                    ontology=self.ontology,
                                                    mode="value_node")
            self.dataloader_value_node = torch.utils.data.DataLoader(value_node_set, batch_size=self.batch_size,
                                                                     shuffle=True)
            self.dataloader_value_node_eval = torch.utils.data.DataLoader(value_node_eval_set,
                                                                          batch_size=value_node_eval_set.ent_num,
                                                                          shuffle=False)
        # ============================================================================================================

        self.use_cuda = torch.cuda.is_available()
        # self.use_cuda = False
        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        print(f"==================== USING {self.device} =====================")
        # ------------------------- Training hyperparameters -----------------------
        self.epoch_num = epoch_num
        self.model = TransEA(dim=self.dim, ent_num=len(self.entity2idx),
                             rel_num=len(self.rel2idx), resume_training=self.resume, device='cuda',
                             dataset_path=self.full_dataset_dir,
                             alpha=0.1)

        self.model.to(self.device)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

    def export_embeddings(self):
        self.write_embeddings(self.model.ent_embedding, "ent_embedding")
        self.write_embeddings(self.model.rel_embedding, "rel_embedding")


    def write_embeddings(self, embedding, embedding_name):
        lines = []
        for embedding in embedding.weight.data:
            line = '\t'.join([str(l) for l in embedding.tolist()])
            lines.append(line)
        content = '\n'.join(lines)
        with open(os.path.join(DATA_DIR, self.full_dataset_dir, f'{embedding_name}.tsv'), 'w') as f:
            f.write(content)
            f.close()

    def inference_evaluation(self):

        total_mrr, counter, filtered_counter, total_fmrr = 0, 0, 0, 0
        hit_rate_list = []
        filtered_hit_rate_list = []
        for test_set in tqdm(self.test_dataloader):
            heads, rels, all_tails, true_tail = test_set[0], test_set[1], test_set[2], test_set[3][0].item()
            selected_idx = (all_tails >= 0)
            heads, rels, all_tails = heads[selected_idx], rels[selected_idx], all_tails[selected_idx]
            triples = torch.stack((heads, rels, all_tails)).type(torch.LongTensor)
            if self.use_cuda:
                distances = self.model.module.infer(triples)
            else:
                distances = self.model.infer(triples)
            f_ranking, f_ranking_idx = evaluate_ranking(distances=distances, all_tails=all_tails,
                                                        true_tail=true_tail)
            filtered_counter += 1
            counter += 1
            total_fmrr += f_ranking
            hit_rate_list.append(f_ranking_idx)
            filtered_hit_rate_list.append(f_ranking_idx)

        filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
        total_fmrr = total_fmrr / filtered_counter
        print("=================== Inference evaluation result ====================")
        print(f"total infer fmrr: {total_fmrr}")
        print(f"filtered infer hit rate: {filtered_hit_rate_list}")
        print("====================================================================")

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
                triples = torch.stack((heads, rels, all_tails)).type(torch.LongTensor).to(self.device)
                distances = self.model.predict_hrt(triples).to(self.device)
                f_ranking, f_ranking_idx = evaluate_ranking(distances=distances, all_tails=all_tails,
                                                            true_tail=true_tail)
                filtered_hit_rate_list.append(f_ranking_idx)
                counter += 1
                total_mrr += f_ranking

            total_mrr = total_mrr / counter
            filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
            print("=================== Training set evaluation ===================")
            print(f"total train mrr: {total_mrr}")
            print(f"the training hit rate list is : {filtered_hit_rate_list}")
            print("===============================================================")

            if self.inference:
                self.inference_evaluation()

    def train(self):
        """
        Split the the training set into non-numerical and numerical subsets
        marked by [3] == -999 or not
        :return:
        """
        self.model.train()
        total_train_loss = 0
        total_numerical_loss = 0
        total_non_numerical_loss = 0

        print("Starting the training")
        for pos, neg in tqdm(self.train_dataloader):
            self.optimizer.zero_grad()
            numerical_idx_list = (pos[3] != -999)
            pos = torch.transpose(torch.stack(pos), 0, 1)
            pos_numerical = pos[numerical_idx_list].to(self.device)
            pos_non_numerical = pos[~numerical_idx_list].to(self.device)
            neg = torch.transpose(torch.stack(neg), 0, 1)
            neg_numerical = neg[numerical_idx_list].to(self.device)
            neg_non_numerical = neg[~numerical_idx_list].to(self.device)
            pos_non_numerical = torch.transpose(pos_non_numerical, 0, 1).long()
            neg_non_numerical = torch.transpose(neg_non_numerical, 0, 1).long()

            pos_numerical = torch.transpose(pos_numerical, 0, 1).long()
            neg_numerical = torch.transpose(neg_numerical, 0, 1).long()

            loss_non_numerical = self.model(pos_non_numerical, neg_non_numerical, numerical_list=None, is_numerical=False)
            loss_non_numerical.mean().backward()
            total_non_numerical_loss += loss_non_numerical.cpu().mean()
            self.optimizer.step()

            if len(pos_numerical) > 0:
                if len(pos_numerical[0]) > 0:
                    if len(pos_numerical) == 1:
                        pos_numerical = pos_numerical.repeat(self.gpu_number, 1)
                        neg_numerical = neg_numerical.repeat(self.gpu_number, 1)
                    loss_numerical = self.model(pos_numerical, neg_numerical, numerical_list=pos_numerical[-1], is_numerical=True) * 0.0001
                    loss_numerical.mean().backward()
                    total_numerical_loss += loss_numerical.cpu().mean()
                    self.optimizer.step()

            self.optimizer.step()

        print(f"Loss: {total_train_loss}")
        print(f"Numerical Loss: {total_numerical_loss}")
        print(f"Non Numerical Loss: {total_non_numerical_loss}")

    def run(self):
        for epoch in range(self.epoch_num + 1):
            print(f"Epoch: {epoch}")
            self.train()
            if epoch % 5 == 0:
                self.scheduler.step()
                self.evaluate()
                if self.value_node_exists:
                    self.calculate_value_node_embedding()
                    self.evaluate_value_node_embedding()
                self.export_embeddings()
                print(f"Current learning rate: {self.scheduler.get_lr()}")

        # if self.inference:
        if self.value_node_exists:
            self.calculate_value_node_embedding()
            self.evaluate_value_node_embedding()
        self.export_embeddings()

    def calculate_value_node_embedding(self):
        with no_grad():
            for triple in tqdm(self.dataloader_value_node):
                if self.use_cuda:
                    self.model.module.calculate_tail_embedding(triple)
                else:
                    self.model.calculate_tail_embedding(triple)

    def evaluate_value_node_embedding(self):
        with no_grad():
            filtered_hit_rate_list = []
            counter = 0
            total_mrr = 0
            for triple in tqdm(self.dataloader_value_node_eval):
                true_tail = triple[3][0].item()
                all_tails = triple[2]
                if self.use_cuda:
                    distances = self.model.module.distance(triple)
                else:
                    distances = self.model.distance(triple)
                f_ranking, f_ranking_idx = evaluate_ranking(distances=distances, all_tails=all_tails,
                                                            true_tail=true_tail)
                filtered_hit_rate_list.append(f_ranking_idx)
                counter += 1
                total_mrr += f_ranking
            total_mrr = total_mrr / counter
            filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
            print("=================== Node value set evaluation ===================")
            print(f"total value node mrr: {total_mrr}")
            print(f"the value node hit rate list is : {filtered_hit_rate_list}")
            print("===============================================================")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dimension", help="dimension of embedding")
    parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
    parser.add_argument("-g", "--gamma", help="gamma for scheduler")
    parser.add_argument("-o", "--ontology", help="main ontology used")
    parser.add_argument("-so", "--sub_ontology", help="name of the sub ontology")
    parser.add_argument("-bs", "--batch_size", help="size of mini batch")
    parser.add_argument("-test", "--test_mode", help="if true, the training will use a smaller training set")
    parser.add_argument("-proj", "--use_projection", help="if true, use projection in numerical linear regression")
    parser.add_argument("-alpha", "--alpha", help="ratio between l_a and l_r")
    parser.add_argument("-margin", "--margin", help="margin for MarginRankLoss")
    parser.add_argument("-epoch", "--epoch", help="number of epochs")
    parser.add_argument("-resume", "--resume", help="resume the training by loading embeddings ")
    parser.add_argument("-global_neg", "--global_neg", help="whether use all entities as negative samples")
    parser.add_argument("-inference", "--inference", help="whether try to do inference with the ontology")
    parser.add_argument("-gpu_num", "--gpu_number", help="number of gpus used")
    args = parser.parse_args()

    gpu_number = 1
    if args.gpu_number:
        gpu_number = int(args.gpu_number)

    dim = 20
    if args.dimension:
        dim = int(args.dimension)

    learning_rate = 0.01
    if args.learning_rate:
        learning_rate = float(args.learning_rate)

    alpha = 0.1
    if args.alpha:
        alpha = float(args.alpha)

    margin = 5
    if args.margin:
        margin = float(args.margin)

    gamma = 1
    if args.gamma:
        gamma = float(args.gamma)

    batch_size = 256
    if args.batch_size:
        batch_size = int(args.batch_size)

    epoch = 100
    if args.epoch:
        epoch = int(args.epoch)

    ontology = "ontospecies_inference"
    if args.ontology:
        ontology = args.ontology

    sub_ontology = "base_full_no_pref_selected_role_limited_100"
    if args.sub_ontology:
        sub_ontology = args.sub_ontology

    test = True
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

    resume = False
    if args.resume:
        if args.resume.lower() == "yes":
            resume = True
        elif args.resume.lower() == "no":
            resume = False
        else:
            resume = False

    global_neg = False
    if args.global_neg:
        if args.global_neg.lower() == "yes":
            global_neg = True
        elif args.global_neg.lower() == "no":
            global_neg = False
        else:
            global_neg = False

    inference = False
    if args.inference:
        if args.inference.lower() == "yes":
            inference = True
        elif args.inference.lower() == "no":
            inference = False
        else:
            inference = False

    print(f"Dimension: {dim}")
    print(f"Learning rate: {learning_rate}")
    print(f"Gamma: {gamma}")
    print(f"Test: {test}")
    print(f"Batch size: {batch_size}")
    print(f"Alpha: {alpha}")
    print(f"Use projection: {use_projection}")
    print(f"Test: {test}")
    print(f"Epoch: {epoch}")
    print(f"Resume training: {resume}")
    print(f"Number of GPUs: {gpu_number}")

    batch_size = batch_size * gpu_number

    if sub_ontology:
        full_dir = os.path.join(DATA_DIR, 'Evaluation', f'{ontology}/{sub_ontology}')
        ontology = sub_ontology
    else:
        full_dir = os.path.join(DATA_DIR, 'Evaluation', f'{ontology}')

    my_trainer = InferenceTrainer(full_dataset_dir=full_dir, ontology=ontology, batch_size=32, dim=dim,
                                  learning_rate=learning_rate, test=test, use_projection=use_projection, alpha=alpha,
                                  margin=margin, epoch_num=epoch, gamma=gamma, resume=resume, inference=inference,
                                  global_neg=global_neg, gpu_number=gpu_number)

    my_trainer.run()
# role_with_subclass_full_attributes_0.1
# role_with_subclass_full_attributes_with_class
