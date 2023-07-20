import os
import pandas as pd
from Marie.Util.location import DATA_DIR


class ScoreAlignmentBalancer:

    def __init__(self):
        self.df_path = os.path.join(DATA_DIR, "CrossGraph/cross_graph_alignment_training.tsv")
        self.df = pd.read_csv(self.df_path, sep="\t")
        # self.df.columns = ["question", "true_score", "true_domain"]
        self.df['true_domain'] = self.df['true_domain'].apply(eval)
        self.domains = self.df['true_domain'].values.tolist()
        # self.df = self.df.drop_duplicates(ignore_index=True)




if __name__ == "__main__":
    balancer = ScoreAlignmentBalancer()
    # balancer.df.to_csv(balancer.df_path, sep="\t", index=False)
    domains = balancer.domains
    value_dict = {}
    for d in domains:
        for idx, value in enumerate(d):
            if value == 1:
                if idx in value_dict:
                    value_dict[idx] += 1
                else:
                    value_dict[idx] = 1

    print(value_dict)