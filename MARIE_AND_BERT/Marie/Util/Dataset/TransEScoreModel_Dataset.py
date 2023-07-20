import torch
import os
import pandas as pd
from torch.utils.data import DataLoader
from torch.utils.data.dataset import Dataset as TorchDataset
from transformers import BertTokenizer

from Marie.Util.CommonTools.FileLoader import FileLoader
from Marie.Util.location import DATA_DIR

# TODO: a Dataset class that provides question examples and their relation
# TODO: also provides a rel embedding

max_len = 12


class Dataset(TorchDataset):
    def __init__(self, df, dataset_dir, mode="general"):
        """
        This dataset provides a train/val set with tokenized question and the correspondent relation embedding
        :param df:
        """
        super(Dataset, self).__init__()
        self.mode = mode
        self.dataset_dir = dataset_dir
        self.df = df
        self.max_len = 12
        # print(self.df)

        all_text = [text for text in self.df.iloc[:, 0]]
        self.all_text = all_text
        if mode == "agent":
            self.tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
            self.agent_name_list = list(set(self.df["agent_name"].values))
            self.ent_embedding_dict, self.idx2entity_dict = self.load_embedding_from_agent(self.agent_name_list)

            agent_name_list = list(self.df["agent_name"].values)
            agent_index_list = list(self.df["agent"].values)
            rel_index_list = list(self.df["rel"].values)
            print(rel_index_list)
            self.agent_y = []
            self.output_y = []


            for agent_name, agent_index, rel_idx, text in zip(agent_name_list, agent_index_list, rel_index_list, all_text):
                # print(agent_name, agent_index, rel_idx)
                # print("rel label", self.idx2entity_dict[agent_name][rel_idx])
                # print("question", text)
                # print("-------------")
                if agent_name == "none":
                    agent_embedding = torch.ones(40)
                    output_embedding = torch.ones(40)
                    self.agent_y.append(torch.FloatTensor(agent_embedding))
                    self.output_y.append(torch.FloatTensor(output_embedding))
                    print("output_y",torch.FloatTensor(output_embedding) )

                    self.dim = 40
                else:
                    agent_embedding = self.ent_embedding_dict[agent_name].iloc[agent_index]
                    output_embedding = self.ent_embedding_dict[agent_name].iloc[rel_idx]
                    self.agent_y.append(torch.FloatTensor(agent_embedding))
                    self.output_y.append(torch.FloatTensor(output_embedding))
                    self.dim = agent_embedding.shape[0]

            self.agent_name_list =  agent_name_list
            self.rel_index_list = rel_index_list
            self.tokenized_questions = [self.tokenizer(text,
                                                       padding='max_length', max_length=self.max_len, truncation=True,
                                                       return_tensors="pt") for text in self.df.iloc[:, 0]]



        else:
            self.tokenizer = BertTokenizer.from_pretrained('bert-base-cased')

            self.tokenized_questions = [self.tokenizer(text,
                                                       padding='max_length', max_length=self.max_len, truncation=True,
                                                       return_tensors="pt") for text in self.df.iloc[:, 0]]
            self.rel_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, 'rel_embedding.tsv'), sep='\t',
                                             header=None)
            self.y = self.rel_embedding.iloc[self.df["rel"].tolist()].reset_index(drop=True)
            self.dim = self.rel_embedding.shape[1]

    def load_embedding_from_agent(self, agent_name_list):
        result = {}
        idx2entity_dict = {}
        for agent_name in agent_name_list:
            if agent_name != "none":
                ent_embedding = pd.read_csv(os.path.join(DATA_DIR, self.dataset_dir, agent_name, 'ent_embedding.tsv'),
                                            sep='\t',
                                            header=None)
                agent_file_loader = FileLoader(full_dataset_dir=os.path.join(DATA_DIR, self.dataset_dir, agent_name))
                entity2idx, idx2entity, rel2idx, idx2rel = agent_file_loader.load_index_files()
                result[agent_name] = ent_embedding
                idx2entity_dict[agent_name] = idx2entity

        return result, idx2entity_dict

    def classes(self):
        return self.y

    def __len__(self):
        return len(self.df)

    def __getitem__(self, idx):
        if self.mode == "agent":
            return self.tokenized_questions[idx], self.agent_y[idx], self.output_y[idx], self.agent_name_list[idx], \
                   self.rel_index_list[idx], self.all_text[idx]
        else:
            return self.tokenized_questions[idx], torch.FloatTensor(self.y.iloc[idx])


if __name__ == "__main__":
    dataset_dir = "CrossGraph/agents"
    df = pd.read_csv(os.path.join(DATA_DIR, dataset_dir, 'score_model_training.tsv'), sep='\t', index_col=0)
    my_set = Dataset(df, dataset_dir=dataset_dir, mode="agent")
    loader = DataLoader(my_set, batch_size=1, shuffle=True)
    for tokenized_question, agent_emb, output_emb in loader:
        print(tokenized_question, agent_emb.shape, output_emb.shape)
