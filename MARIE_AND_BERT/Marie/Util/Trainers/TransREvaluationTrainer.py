import os, sys
import pickle
import time

sys.path.append("")
sys.path.append("../../..")
import pandas as pd
import torch
from torch import no_grad
from torch.optim.lr_scheduler import ExponentialLR
from torch.utils.data import DataLoader
from tqdm import tqdm

from Marie.Util.Dataset.LinkPredictionDataset import LinkPredictionDataset
from Marie.Util.Models.TransR import TransR
from Marie.Util.location import DATA_DIR


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


class TransREvaluationTrainer:
    """
    The sole purpose is to make sure that the current implementation of the TransR and TransRA embedding
    produce similar benchmark scores in relation prediction on FB15K dataset (Required by MK306)
    """

    def __init__(self, dataset_dir=None, dataset_name=None, mode="transr", dim=50, learning_rate=0.1, gamma=0.1,
                 epoch_num=500, batch_size=32, margin=5, neg_rate=20, test=False):
        self.mode = mode.lower().strip()
        self.test = test
        # load dataset and dataloader
        self.dim = dim
        self.margin = margin
        self.batch_size = batch_size
        self.dataset_dir = dataset_dir
        self.dataset_name = dataset_name
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.epoch_num = epoch_num
        self.neg_rate = neg_rate
        # ================= load cached hr_dict ==========
        self.hr_dict = pickle.load(open(os.path.join(dataset_dir, "hr_dict.pkl"), "rb"))

        # ==================== load train/valid/test ======

        df_train = pd.read_csv(os.path.join(dataset_dir, "fb15k-train.txt"), sep="\t", header=None)
        df_test = pd.read_csv(os.path.join(dataset_dir, "fb15k-test.txt"), sep="\t", header=None)

        if self.test:
            df_train = df_train.sample(frac=0.0001)
            # df_test = df_test.sample(frac=0.001)
        # df_valid = pd.read_csv(os.path.join(dataset_dir, "fb15k-valid.txt"), sep="\t", header=None)

        train_set = LinkPredictionDataset(df_train, dataset_path=dataset_dir, dataset_name="fb15k",
                                          neg_rate=self.neg_rate)
        self.rel_num, self.ent_num = train_set.rel_num, train_set.ent_num
        self.train_dataloader = DataLoader(train_set, shuffle=True, batch_size=self.batch_size)
        # valid_set = LinkPredictionDataset(df_valid, dataset_path=dataset_dir, dataset_name="fb15k",
        # neg_rate=self.rel_num, mode="test")
        test_set = LinkPredictionDataset(df_test, dataset_path=dataset_dir, dataset_name="fb15k",
                                         neg_rate=self.rel_num, mode="test")
        # self.valid_dataloader = DataLoader(valid_set, shuffle=False, batch_size=self.rel_num)
        self.test_dataloader = DataLoader(test_set, shuffle=False, batch_size=1)
        # =================================================

        # ===================== device ====================
        self.use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        # =================================================

        # load the model (TransR/TransRA) according to the mode parameter
        if self.mode == "transr":
            # disable numerical prediction in TransR model
            self.model = TransR(rel_dim=dim, rel_num=self.rel_num, ent_dim=dim, ent_num=self.ent_num,
                                device=self.device,
                                use_projection=False, alpha=0.1,
                                margin=self.margin, resume_training=False, dataset_path=self.dataset_dir,
                                enable_numerical=False)
        else:
            pass
            # enable numerical prediction in TransR model, then it becomes TransRA ...

        # ================== optimizer and scheduler ======
        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)
        # =================================================

    def evaluate_ranking(self, distances, all_tails, true_tail, head_idx, rel_idx):

        h_r = f"{head_idx.item()}_{rel_idx.item()}"
        other_true_candidates = list(self.hr_dict[h_r])
        other_true_candidates.remove(true_tail)
        other_true_candidates = torch.LongTensor(other_true_candidates)

        _, B = torch.topk(distances, k=len(distances), largest=False)
        ranked_candidates = all_tails[B]
        # remove all other true tails from the all_tails
        ranked_candidates = ranked_candidates[~other_true_candidates.unsqueeze(1).eq(ranked_candidates).any(0)].long()
        pred_idx = (ranked_candidates == true_tail).nonzero(as_tuple=True)[0]
        f_ranking_idx = pred_idx
        f_ranking = 1 / (f_ranking_idx + 1)

        return f_ranking, f_ranking_idx

    def write_embeddings(self, embedding, embedding_name):
        lines = []
        for embedding in embedding.weight.data:
            line = '\t'.join([str(l) for l in embedding.tolist()])
            lines.append(line)
        content = '\n'.join(lines)
        with open(os.path.join(DATA_DIR, self.dataset_dir, f'{embedding_name}.tsv'), 'w') as f:
            f.write(content)
            f.close()

    def export_embeddings(self):
        self.write_embeddings(self.model.ent_embedding, "ent_embedding")
        self.write_embeddings(self.model.rel_embedding, "rel_embedding")
        self.write_embeddings(self.model.attr_embedding, "attr_embedding")
        self.write_embeddings(self.model.bias_embedding, "bias_embedding")
        self.write_embeddings(self.model.proj_matrix, "proj_matrix")

    def evaluate(self):
        with no_grad():
            self.model.eval()
            total_mrr = 0
            counter = 0
            filtered_hit_rate_list = []
            print("Evaluating ...")
            for test_set in tqdm(self.test_dataloader):
                head_idx, rel_idx, true_tail = test_set
                heads = head_idx.repeat(self.ent_num)
                rels = rel_idx.repeat(self.ent_num)
                tails = torch.range(0, self.ent_num - 1).long()
                triples = torch.stack((heads, rels, tails)).type(torch.LongTensor)
                distances = self.model.predict(triples=triples)
                f_ranking, f_ranking_idx = self.evaluate_ranking(distances=distances, all_tails=tails,
                                                                 true_tail=true_tail, head_idx=head_idx,
                                                                 rel_idx=rel_idx)

                filtered_hit_rate_list.append(f_ranking_idx)
                counter += 1
                total_mrr += f_ranking
                # print(f"To calculate the ranking: {time.time() - START_TIME}")

            total_mrr = total_mrr / counter
            filtered_hit_rate_list = hit_rate(filtered_hit_rate_list)
            print("=================== Training set evaluation ===================")
            print(f"total train mrr: {total_mrr}")
            print(f"the training hit rate list is : {filtered_hit_rate_list}")
            print("===============================================================")

    def train(self):
        print("Training ...")
        total_train_loss = 0
        for pos_triple, neg_triple in tqdm(self.train_dataloader):
            pos_triple = torch.transpose(torch.stack(pos_triple), 0, 1)
            neg_triple = torch.transpose(torch.stack(neg_triple), 0, 1)

            self.optimizer.zero_grad()
            loss = self.model(pos_triple, neg_triple)
            loss.mean().backward()
            total_train_loss += loss.mean().cpu()
            self.optimizer.step()
            self.model.normalize_parameters()
        print(f"total train loss: {total_train_loss}")

    def run(self):
        for epoch in range(self.epoch_num):
            if epoch % 10 == 0:
                self.evaluate()
                self.export_embeddings()
            self.train()

        self.export_embeddings()


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dimension", help="dimension of embedding")
    parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
    parser.add_argument("-g", "--gamma", help="gamma for scheduler")
    parser.add_argument("-bs", "--batch_size", help="size of mini batch")
    parser.add_argument("-margin", "--margin", help="margin for MarginRankLoss")
    parser.add_argument("-nr", "--neg_rate", help="number of negative samples")
    parser.add_argument("-epoch", "--epoch", help="number of epochs")

    args = parser.parse_args()

    dim = 20
    if args.dimension:
        dim = int(args.dimension)

    learning_rate = 0.01
    if args.learning_rate:
        learning_rate = float(args.learning_rate)

    neg_rate = 20
    if args.neg_rate:
        neg_rate = int(args.neg_rate)

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

    dataset_dir = os.path.join(DATA_DIR, "CrossGraph", "fb15k")
    my_trainer = TransREvaluationTrainer(dataset_dir=dataset_dir, dataset_name="fb15k",
                                         epoch_num=epoch, dim=dim, gamma=gamma, learning_rate=learning_rate,
                                         batch_size=batch_size, margin=margin, neg_rate=neg_rate)
    my_trainer.run()
