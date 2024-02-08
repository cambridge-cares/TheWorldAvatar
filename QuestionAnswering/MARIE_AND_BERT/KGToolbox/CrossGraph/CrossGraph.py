import os

import pandas as pd
import torch
from torch.nn.functional import one_hot

from Marie.Util.location import DATA_DIR

label_dict = {"pubchem": 0, "ontocompchem": 1, "ontospecies": 2,
              "ontokin": 3, "wikidata": 4, "ontospecies_new": 5,
              "ontoagent": 6, "ontomops": 7, "ontokin_reaction": 8}

label_list = list(label_dict.keys())

# training_dataset_path = os.path.join(DATA_DIR, "CrossGraph", "cross_graph_alignment_training.tsv")
#
# df = pd.read_csv(training_dataset_path, sep="\t", index_col=None)
#
# all_rows = []
# for question, domain_list in zip(df['question'].values, df['true_domain'].apply(eval).values):
#     domain_label_list = []
#     for domain, domain_label in zip(domain_list, label_list):
#         if domain == 1:
#             domain_label_list.append(domain_label)
#     all_rows.append((question, domain_label_list))
#
# new_df = pd.DataFrame(all_rows)
# new_df.columns = ["question", "true_domain"]
# new_df.to_csv(os.path.join(DATA_DIR, "CrossGraph", "cross_graph_alignment_training_labelled.tsv"), sep="\t")

df_path = os.path.join(DATA_DIR, "CrossGraph", "cross_graph_alignment_training_labelled.tsv")
df_labelled = pd.read_csv(df_path, sep="\t")
df_labelled["true_domain"].apply(eval)

all_rows = []

for question, domain_list in zip(df_labelled['question'].values, df_labelled['true_domain'].apply(eval).values):
    print(question)
    domain_index_list = [label_dict[d] for d in domain_list]
    print(domain_index_list)
    encoded_domain_index = torch.sum(one_hot(torch.tensor(domain_index_list), num_classes=len(label_dict.keys())),
                                     dim=0)
    all_rows.append((question, 1, encoded_domain_index.tolist()))

df_final = pd.DataFrame(all_rows)
df_final.columns = ["question", "true_score", "true_domain"]
df_final.to_csv(os.path.join(DATA_DIR, "CrossGraph", "cross_graph_alignment_training_updated.tsv"), sep="\t", index=False)
