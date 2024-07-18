import json
import os, sys

sys.path.append('')
sys.path.append('../../Marie/Util/')
sys.path.append('../../Marie/')
sys.path.append("../..")
import torch
from torch.optim.lr_scheduler import ExponentialLR
from tqdm import tqdm
from torch import no_grad

from Marie.Util.Dataset.TransEA_Dataset import Dataset
from Marie.Util.Models.TransEA import TransEA
from Marie.Util.NHopExtractor import HopExtractor

from Marie.Util.location import DATA_DIR
from Marie.Util.location import EVALUATION_DIR


class TransEATrainer:
    """
    TransEA training comes with three parts of datasets: postive sets, negative sets, and attribute sets - all
    created on top of triples.
    for certain batches of positive sets,
    """

    def __init__(self, dataset_path, dataset_name, dim, epoch_num, learning_rate=1.0, gamma=1.0, batch_size=128,
                 test_step=100, alpha=0.1, resume_training=True, enable_numerical=False):
        self.dataset_path = dataset_path
        self.dataset_name = dataset_name
        self.dim = dim
        self.batch_size = batch_size
        self.epoch_num = epoch_num
        self.test_step = test_step
        self.learning_rate = learning_rate
        self.gamma = gamma
        self.step = 0
        self.alpha = alpha
        self.enable_numerical = enable_numerical

        train_triplets = [line.split('\t') for line in
                          open(os.path.join(DATA_DIR, self.dataset_path,
                                            f'{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets = [line.split('\t') for line in
                         open(os.path.join(DATA_DIR, self.dataset_path,
                                           f'{self.dataset_name}-test.txt')).read().splitlines()]

        self.train_set = Dataset(train_triplets, dataset_path=self.dataset_path, dataset_name=dataset_name)
        self.test_set = Dataset(test_triplets, dataset_path=self.dataset_path, dataset_name=dataset_name)

        self.e_num = self.train_set.ent_num
        self.r_num = self.train_set.rel_num
        self.use_cuda = torch.cuda.is_available()
        device = torch.device("cuda" if self.use_cuda else "cpu")
        self.device = device
        print(f'=========== USING {device} ===============')
        self.model = TransEA(dim=self.dim, ent_num=self.e_num, rel_num=self.r_num,
                             resume_training=resume_training, device=device, dataset_path=dataset_path,
                             alpha=self.alpha)
        self.optimizer = torch.optim.SGD(self.model.parameters(), lr=self.learning_rate)
        self.scheduler = ExponentialLR(self.optimizer, gamma=self.gamma)
        self.hop_extractor = HopExtractor(dataset_dir=os.path.join(DATA_DIR, self.dataset_path),
                                          dataset_name=self.dataset_name)

    def get_train_data(self):
        # wikidata_single_full_numerical-train
        train_triplets_non_numerical = [line.split('\t') for line in
                                        open(os.path.join(DATA_DIR, self.dataset_path,
                                                          f'{self.dataset_name}-train.txt')).read().splitlines()]

        test_triplets_non_numerical = [line.split('\t') for line in
                                       open(os.path.join(DATA_DIR, self.dataset_path,
                                                         f'{self.dataset_name}-test.txt')).read().splitlines()]

        train_non_numerical_set = Dataset(train_triplets_non_numerical, dataset_path=self.dataset_path,
                                          dataset_name=self.dataset_name,
                                          is_numerical=False)
        test_non_numerical_set = Dataset(test_triplets_non_numerical, dataset_path=self.dataset_path,
                                         dataset_name=self.dataset_name,
                                         is_numerical=False)
        train_non_numerical_dataloader = torch.utils.data.DataLoader(train_non_numerical_set,
                                                                     batch_size=self.batch_size,
                                                                     shuffle=True)
        test_non_numerical_dataloader = torch.utils.data.DataLoader(test_non_numerical_set, batch_size=self.batch_size,
                                                                    shuffle=True)
        if self.enable_numerical:
            train_triplets_numerical = [line.split('\t') for line in
                                        open(os.path.join(DATA_DIR, self.dataset_path,
                                                          f'{self.dataset_name}_numerical-train.txt')).read().splitlines()]

            test_triplets_numerical = [line.split('\t') for line in
                                       open(os.path.join(DATA_DIR, self.dataset_path,
                                                         f'{self.dataset_name}_numerical-test.txt')).read().splitlines()]

            train_numerical_set = Dataset(train_triplets_numerical, dataset_path=self.dataset_path, is_numerical=True)
            test_numerical_set = Dataset(test_triplets_numerical, dataset_path=self.dataset_path, is_numerical=True)
            train_numerical_dataloader = torch.utils.data.DataLoader(train_numerical_set, batch_size=self.batch_size,
                                                                     shuffle=True)
            test_numerical_dataloader = torch.utils.data.DataLoader(test_numerical_set, batch_size=self.batch_size,
                                                                    shuffle=True)
            return train_numerical_dataloader, train_non_numerical_dataloader, \
                   test_numerical_dataloader, test_non_numerical_dataloader
        else:
            return train_non_numerical_dataloader, test_non_numerical_dataloader

    def hit_at_k(self, predictions, ground_truth_idx, k: int = 10, largest=False):
        k = min(k, len(predictions))
        _, indices_top_k = torch.topk(predictions, k=k, largest=largest)
        if ground_truth_idx in indices_top_k:
            return 1
        else:
            return 0

    def evaluate(self, test_dataloader, is_numerical=True):
        with no_grad():
            self.model.eval()
            total_diff = 0
            total_loss_val = 0
            hit_10 = 0
            hit_5 = 0
            hit_1 = 0
            total_case = 0
            counter = 0
            for pos_triples, neg_triples, numerical_list in test_dataloader:
                prediction = self.model.predict_hrt(pos_triples).mean()
                total_loss_val += prediction
                ground_truth_triplets = torch.transpose(torch.stack(pos_triples), 0, 1).type(torch.LongTensor)
                for i, triplet in enumerate(ground_truth_triplets):
                    head = triplet[0]
                    rel = triplet[1]
                    tail_true = triplet[2]

                    tail_all = self.hop_extractor.extract_neighbour_from_idx(head.item())
                    if tail_true.item() not in tail_all:
                        tail_all.append(tail_true.item())
                    tail_true_idx = tail_all.index(tail_true.item())
                    tail_all = torch.LongTensor(tail_all)

                    # tail_all = torch.range(0, self.e_num - 1).type(torch.LongTensor)
                    # tail_true_idx = tail_all.tolist().index(tail_true.item())
                    head_tensor = head.repeat(len(tail_all))
                    rel_tensor = rel.repeat(len(tail_all))
                    # tail_all = torch.range(0, self.e_num - 1).type(torch.LongTensor)
                    new_triplets = torch.stack((head_tensor, rel_tensor, tail_all)).type(torch.LongTensor)
                    prediction = self.model.predict_hrt(new_triplets)

                    total_case += 1
                    hit_10 += self.hit_at_k(prediction.to(self.device), tail_true_idx, k=10)
                    hit_5 += self.hit_at_k(prediction.to(self.device), tail_true_idx, k=5)
                    hit_1 += self.hit_at_k(prediction.to(self.device), tail_true_idx, k=1)

                if is_numerical:
                    predicted_numerical = self.model.predict_numeric(pos_triples).cpu()
                    average_diff = torch.abs(predicted_numerical - numerical_list.cpu())
                    print("------------------------------------")
                    print("predicted_numerical", predicted_numerical)
                    print("numerical_list", numerical_list)
                    total_diff += average_diff.sum().item() / len(numerical_list)
                    if counter == 0:
                        with open("test_predicted.json", "w") as f:
                            f.write(json.dumps(predicted_numerical.tolist()))
                            f.close()

                        with open("test_true.json", "w") as f:
                            f.write(json.dumps(numerical_list.tolist()))
                            f.close()
                    counter += 1

            hit_10_rate = hit_10 / total_case
            hit_5_rate = hit_5 / total_case
            hit_1_rate = hit_1 / total_case
            print(
                '=======================================================================================================')
            print('Current Hit 10 rate:', hit_10, ' out of ', total_case, ' ratio is: ', hit_10_rate)
            print('Current Hit 5 rate:', hit_5, ' out of ', total_case, ' ratio is: ', hit_5_rate)
            print('Current Hit 1 rate:', hit_1, ' out of ', total_case, ' ratio is: ', hit_1_rate)
            print(f'total_loss_val {total_loss_val}')
            if is_numerical:
                print("========= Numerical valuation result =========")
                print(f"total diff error: {total_diff / len(test_dataloader)}")
                print("=====================================")

    def write_embeddings(self, embedding, embedding_name):
        lines = []
        for embedding in embedding.weight.data:
            line = '\t'.join([str(l) for l in embedding.tolist()])
            lines.append(line)
        content = '\n'.join(lines)
        with open(os.path.join(DATA_DIR, self.dataset_path, f'{embedding_name}.tsv'), 'w') as f:
            f.write(content)
            f.close()

    def export_embeddings(self):
        self.write_embeddings(self.model.ent_embedding, "ent_embedding")
        self.write_embeddings(self.model.rel_embedding, "rel_embedding")
        self.write_embeddings(self.model.attr_embedding, "attr_embedding")
        self.write_embeddings(self.model.bias, "bias_embedding")

    def run(self):
        if self.enable_numerical:
            train_numerical_dataloader, train_non_numerical_dataloader, \
            test_numerical_dataloader, test_non_numerical_dataloader = self.get_train_data()
        else:
            train_non_numerical_dataloader, test_non_numerical_dataloader = self.get_train_data()
        with tqdm(total=self.epoch_num, unit=' epoch') as tepoch:

            # train numerical datasets first
            total_loss_train = 0
            for epoch in range(self.epoch_num):
                total_loss_train = 0
                self.model.train()
                for pos_triples, neg_triples, _ in tqdm(train_non_numerical_dataloader):
                    self.optimizer.zero_grad()
                    loss = self.model(positive_triplets=pos_triples, negative_triplets=neg_triples,
                                      numerical_list=None, is_numerical=False)
                    loss.backward()
                    total_loss_train += loss.mean().item()
                    self.optimizer.step()
                    self.step += 1

                if self.enable_numerical:
                    for pos_triples, neg_triples, numerical_list in train_numerical_dataloader:
                        self.optimizer.zero_grad()
                        loss = self.model(positive_triplets=pos_triples, negative_triplets=neg_triples,
                                          numerical_list=numerical_list, is_numerical=True)
                        loss.backward()
                        total_loss_train += loss.mean().item()
                        self.optimizer.step()
                        self.step += 1

                if (epoch + 1) % 500 == 0:
                    self.scheduler.step()

                if (epoch + 1) % self.test_step == 0:
                    if self.enable_numerical:
                        self.evaluate(test_numerical_dataloader, is_numerical=True)
                    self.evaluate(test_non_numerical_dataloader, is_numerical=False)
                    print(f"Learning rate changed to {self.scheduler.get_lr()}")
                    print("Exporting embedding")
                    self.export_embeddings()

                print(f"Epoch {epoch} -  loss: {total_loss_train}")


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("-d", "--dimension", help="dimension of embedding")
    parser.add_argument("-lr", "--learning_rate", help="starting learning rate")
    parser.add_argument("-g", "--gamma", help="gamma for scheduler")
    parser.add_argument("-o", "--ontology", help="main ontology used")
    parser.add_argument("-bs", "--batch_size", help="size of mini batch")
    parser.add_argument("-test", "--test_mode", help="if true, the training will use a smaller training set")
    parser.add_argument("-proj", "--use_projection", help="if true, use projection in numerical linear regression")
    parser.add_argument("-alpha", "--alpha", help="ratio between l_a and l_r")
    parser.add_argument("-margin", "--margin", help="margin for MarginRankLoss")
    parser.add_argument("-epoch", "--epoch", help="number of epochs")
    parser.add_argument("-resume", "--resume", help="resume the training by loading embeddings ")
    parser.add_argument("-global_neg", "--global_neg", help="whether use all entities as negative samples")
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

    gamma = 1
    if args.gamma:
        gamma = float(args.gamma)

    batch_size = 256
    if args.batch_size:
        batch_size = int(args.batch_size)

    epoch = 100
    if args.epoch:
        epoch = int(args.epoch)

    ontology = "pubchem"
    if args.ontology:
        ontology = args.ontology

    resume = False
    if args.resume:
        if args.resume.lower() == "yes":
            resume = True
        elif args.resume.lower() == "no":
            resume = False
        else:
            resume = False

    print(f"Dimension: {dim}")
    print(f"Learning rate: {learning_rate}")
    print(f"Gamma: {gamma}")
    print(f"Batch size: {batch_size}")
    print(f"Alpha: {alpha}")
    print(f"Epoch: {epoch}")
    print(f"Resume training: {resume}")
    print(f"Number of GPUs: {gpu_number}")

    batch_size = batch_size * gpu_number
    test_step = 100
    print("============== STARTING TRAINING WITH PARAMETERS ======================")
    print(f"alpha: {alpha}")
    print(f"gamma: {gamma}")
    print(f"lr: {learning_rate}")
    print(f"dim: {dim}")
    print(f"batch size: {batch_size}")
    trainer = TransEATrainer(dataset_path=os.path.join(DATA_DIR, "CrossGraph", ontology),
                             dataset_name=ontology, dim=dim, epoch_num=epoch,
                             learning_rate=learning_rate, batch_size=batch_size, gamma=gamma, test_step=test_step,
                             alpha=alpha, resume_training=resume)
    print("Starting the training")
    trainer.run()
