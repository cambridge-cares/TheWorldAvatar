import json
import random
import sys

from torch import no_grad, nn
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm

sys.path.append("../..")

import os
import pandas as pd
import torch
from Marie.Util.Models.Complex import Complex
from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.Dataset.Complex_Inference_Dataset import ComplexInferenceDataset
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


class ComplexTrainer:

    def __init__(self, full_dataset_dir, ontology, batch_size=32, epoch_num=100, dim=20, learning_rate=1.0, gamma=1,
                 test=False, use_projection=False, alpha=0.1, margin=5, resume=False, inference=True, global_neg=False,
                 gpu_number=1, is_numerical=False):

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
        self.is_numerical = is_numerical
        self.inference = inference
        self.my_extractor = HopExtractor(
            dataset_dir=self.full_dataset_dir,
            dataset_name=self.ontology)

        if os.path.exists(os.path.join(full_dir, f"{self.ontology}-train-2.txt")):
            df_train = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-train-2.txt"), sep="\t", header=None)
            self.use_label_dict = json.loads(open(f"{self.full_dataset_dir}/use_label_dict.json").read())
        else:
            df_train = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-train.txt"), sep="\t", header=None)
        self.df_train = df_train
        df_train_small = df_train.sample(frac=0.01)

        self.file_loader = FileLoader(full_dataset_dir=self.full_dataset_dir, dataset_name=self.ontology)
        self.entity2idx, self.idx2entity, self.rel2idx, self.idx2rel = self.file_loader.load_index_files()
        self.ent_num = len(self.entity2idx.keys())
        self.rel_num = len(self.rel2idx.keys())
        numerical_eval_path = os.path.join(full_dir, f"numerical_eval.tsv")

        # ============================== TODO: clean up the training data loading mechanism ============================
        # ========================================== CREATE DATASET FOR TRAINING ================================
        # 1. if test is used, use the df_train_small instead of the full dataset
        # 2. Inference flag should only effect whether the evaluation uses inference
        # =======================================================================================================

        # ================================== Load test set for inference =======================================
        if self.inference:
            df_test = pd.read_csv(os.path.join(full_dir, f"{self.ontology}-test.txt"), sep="\t", header=None)
            test_set = ComplexInferenceDataset(df_test, full_dataset_dir=self.full_dataset_dir,
                                               ontology=self.ontology,
                                               mode="test", global_neg=self.global_neg)
            self.test_dataloader = torch.utils.data.DataLoader(test_set,
                                                               batch_size=test_set.candidate_max * self.gpu_number,
                                                               shuffle=False)
        # ================================== Load training set for general embedding ============================
        if self.test:
            print("Using small dataset for testing")
            df_train = df_train_small

        train_set = ComplexInferenceDataset(df_train, full_dataset_dir=self.full_dataset_dir,
                                            ontology=self.ontology,
                                            mode="general_train", global_neg=self.global_neg)
        self.train_dataloader = torch.utils.data.DataLoader(train_set, batch_size=batch_size, shuffle=True)

        # ==================================== Load evaluation set, which is a smaller training set ===============
        train_set_eval = ComplexInferenceDataset(df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                 ontology=self.ontology,
                                                 mode="general_train_eval", global_neg=self.global_neg)
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
        else:
            print("Singular node trianing set not exists", value_node_path)
            df_value_node = self.df_train

        if self.is_numerical:
            eval_mode = "value_node_eval"
            value_node_eval_mode = "value_node"
        else:
            eval_mode = "general_train_eval"
            value_node_eval_mode = "general_test"

        value_node_eval_set = ComplexInferenceDataset(df=df_train_small, full_dataset_dir=self.full_dataset_dir,
                                                      ontology=self.ontology,
                                                      mode=eval_mode, global_neg=self.global_neg)
        value_node_set = ComplexInferenceDataset(df=df_value_node, full_dataset_dir=self.full_dataset_dir,
                                                 ontology=self.ontology,
                                                 mode=value_node_eval_mode, global_neg=self.global_neg)
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
        self.model = Complex(self.dim, ent_num=self.ent_num, rel_num=self.rel_num, resume_training=self.resume,
                             device=self.device,
                             dataset_dir=self.full_dataset_dir, enable_numerical=True)

        if self.gpu_number > 1:
            self.model = nn.DataParallel(self.model)
        self.model.to(self.device)

        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)

    def evaluate_ranking(self, distances, all_tails, true_tail, show_idx=False):

        _, B = torch.topk(distances, k=len(distances), largest=True)
        B = B.tolist()
        selected_candidates = [all_tails.tolist()[idx] for idx in B]
        if true_tail in selected_candidates:
            f_ranking_idx = selected_candidates.index(true_tail)
            if f_ranking_idx == 0:
                # this is a direct hit
                if show_idx:
                    print("Selected out of ", len(selected_candidates))
                    print("Hit 1 entity")
                    label = self.idx2entity[true_tail]
                    print(self.use_label_dict[label])
                    print("----------------------------")

            if f_ranking_idx < 5:
                if show_idx:
                    print("Selected out of ", len(selected_candidates))
                    print("Hit 5 entity")
                    label = self.idx2entity[true_tail]
                    print(self.use_label_dict[label])
                    print("----------------------------")

            f_ranking = 1 / (f_ranking_idx + 1)
        else:
            f_ranking = 0
            f_ranking_idx = -1
        return f_ranking, f_ranking_idx

    def export_embeddings(self):

        ent_lines = []
        for re_ent, im_ent in zip(self.model.re_ent.weight.data, self.model.im_ent.weight.data):
            e_line = '\t'.join([str(e) for e in re_ent.tolist()] + [str(e) for e in im_ent.tolist()])
            ent_lines.append(e_line)

        ent_content = '\n'.join(ent_lines)
        with open(os.path.join(DATA_DIR, f'{self.full_dataset_dir}/ent_embedding.tsv'), 'w') as f:
            f.write(ent_content)
            f.close()

        rel_lines = []
        for re_rel, im_rel in zip(self.model.re_rel.weight.data, self.model.im_rel.weight.data):
            r_line = '\t'.join([str(e) for e in re_rel.tolist()] + [str(e) for e in im_rel.tolist()])
            rel_lines.append(r_line)
        rel_content = '\n'.join(rel_lines)
        with open(os.path.join(DATA_DIR, f'{self.full_dataset_dir}/rel_embedding.tsv'), 'w') as f:
            f.write(rel_content)
            f.close()

        if self.gpu_number > 1:
            # self.write_embeddings(self.model.module.ent_embedding, "ent_embedding")
            # self.write_embeddings(self.model.module.rel_embedding, "rel_embedding")
            self.write_embeddings(self.model.module.attr, "attr_embedding")
            self.write_embeddings(self.model.module.bias, "bias_embedding")
        else:
            # self.write_embeddings(self.model.ent_embedding, "ent_embedding")
            # self.write_embeddings(self.model.rel_embedding, "rel_embedding")
            self.write_embeddings(self.model.attr, "attr_embedding")
            self.write_embeddings(self.model.bias, "bias_embedding")

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
            if self.gpu_number > 1:
                distances = self.model.module.infer(triples)
            else:
                distances = self.model.infer(triples)
            f_ranking, f_ranking_idx = self.evaluate_ranking(distances=distances, all_tails=all_tails,
                                                             true_tail=true_tail, show_idx=True)
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
                triples = torch.stack((heads, rels, all_tails)).type(torch.LongTensor)
                if self.gpu_number > 1:
                    distances = self.model.module.predict(triples)
                else:
                    distances = self.model.predict(triples)
                f_ranking, f_ranking_idx = self.evaluate_ranking(distances=distances, all_tails=all_tails,
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
        for triples in tqdm(self.train_dataloader):
            self.optimizer.zero_grad()
            numerical_idx_list = (triples[4] != -999)
            triples = torch.transpose(torch.stack(triples), 0, 1)
            triples_non_numerical = triples[~numerical_idx_list].to(self.device)
            loss_non_numerical = self.model(triples_non_numerical, mode="non_numerical")
            loss_non_numerical.mean().backward()
            total_non_numerical_loss += loss_non_numerical.cpu().mean()
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
                self.export_embeddings()
                print(f"Current learning rate: {self.scheduler.get_lr()}")

        self.export_embeddings()

    def calculate_value_node_embedding(self):
        with no_grad():
            for triple in tqdm(self.dataloader_value_node):
                if self.gpu_number > 1:
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
                if self.gpu_number > 1:
                    distances = self.model.module.distance(triple)
                else:
                    distances = self.model.distance(triple)
                f_ranking, f_ranking_idx = self.evaluate_ranking(distances=distances, all_tails=all_tails,
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
    parser.add_argument("-is_numerical", "--is_numerical", help="whether enable numerical embedding")
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

    ontology = "ontospecies_new"
    if args.ontology:
        ontology = args.ontology

    sub_ontology = None
    if args.sub_ontology:
        sub_ontology = args.sub_ontology

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

    is_numerical = False
    if args.is_numerical:
        if args.is_numerical.lower() == "yes":
            is_numerical = True
        elif args.is_numerical.lower() == "no":
            is_numerical = False
        else:
            is_numerical = False

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
        full_dir = os.path.join(DATA_DIR, 'CrossGraph', f'{ontology}/{sub_ontology}')
        ontology = sub_ontology
    else:
        full_dir = os.path.join(DATA_DIR, 'CrossGraph', f'{ontology}')

    my_trainer = ComplexTrainer(full_dataset_dir=full_dir, ontology=ontology, batch_size=32, dim=dim,
                                learning_rate=learning_rate, test=test, use_projection=use_projection, alpha=alpha,
                                margin=margin, epoch_num=epoch, gamma=gamma, resume=resume, inference=inference,
                                global_neg=global_neg, gpu_number=gpu_number, is_numerical=is_numerical)

    my_trainer.run()
