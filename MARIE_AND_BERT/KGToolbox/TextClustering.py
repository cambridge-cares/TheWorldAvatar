import json
import os
import time

from leven import levenshtein
import numpy as np
from sklearn.cluster import dbscan
import sys

sys.path.append("..")
from Marie.Util.location import DATA_DIR


def cluster_text(data, save_path):
    def lev_metric(x, y):
        i, j = int(x[0]), int(y[0])  # extract indices
        return levenshtein(data[i], data[j])

    X = np.arange(len(data)).reshape(-1, 1)
    _, rst = dbscan(X, metric=lev_metric, eps=5, min_samples=4000, n_jobs=16)
    rst_list = rst.tolist()
    with open(save_path, "w") as f:
        f.write(json.dumps(rst_list))
        f.close()


class TextClustering:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.text_file_path = os.path.join(self.full_dataset_dir, "reaction_text_list.json")
        self.text_file = list(json.loads(open(self.text_file_path).read()))

    def run(self):
        START_TIME = time.time()
        test_length = 1000
        cluster_text(self.text_file[0:test_length],
                     save_path=os.path.join(self.full_dataset_dir, "text_clustering.json"))
        time_used = time.time() - START_TIME
        print("Time used:", time_used)
        est_time = (len(self.text_file) / test_length) ** 2 * time_used
        print("estimated time", est_time / 3600, "in hours")


if __name__ == "__main__":
    text_cluster = TextClustering()
    text_cluster.run()
