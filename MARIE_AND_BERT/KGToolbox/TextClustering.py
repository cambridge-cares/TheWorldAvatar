import json
import math
import os
import pickle
import numpy as np
from matplotlib import pyplot as plt
from sklearn.cluster import dbscan
import sys

sys.path.append("..")
from Marie.Util.location import DATA_DIR


def over_lapping_metric(i, j, reaction_species_dict):
    s_list_1 = set(reaction_species_dict[str(i)])
    s_list_2 = set(reaction_species_dict[str(j)])
    l_1 = len(s_list_1)
    l_2 = len(s_list_2)
    distance = round(len(s_list_1 ^ s_list_2) / (l_1 + l_2), 5) * 100
    return distance


class TextClustering:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.distance_matrix_path = os.path.join(self.full_dataset_dir, "distance_matrix.pkl")
        if os.path.exists(self.distance_matrix_path):
            with open(self.distance_matrix_path, 'rb') as handle:
                self.distance_matrix = pickle.load(handle)
        else:
            self.distance_matrix = self.make_distance_matrix()
            with open(self.distance_matrix_path, 'wb') as handle:
                pickle.dump(self.distance_matrix, handle, protocol=pickle.HIGHEST_PROTOCOL)

    def run(self):
        pass

    def make_distance_matrix(self):
        reaction_idx_list = json.loads(open(f"{self.full_dataset_dir}/reaction_idx_list.json").read())
        reaction_species_dict = json.loads(open(f"{self.full_dataset_dir}/reaction_species_mapping.json").read())
        test_length = 20000
        reaction_idx_list = reaction_idx_list[0:test_length]
        counter = 0
        list_length = len(reaction_idx_list)
        matrix = []
        for i in reaction_idx_list:
            counter += 1
            progress = round(counter / list_length * 100, 2)
            print(f"{progress} %")
            row = []
            for j in reaction_idx_list:
                d = over_lapping_metric(i, j, reaction_species_dict=reaction_species_dict)
                row.append(d)
            matrix.append(row)
        return matrix

    def cluster_by_precomputed_matrix(self):
        X = self.distance_matrix
        X = np.array(X)
        print(X.shape)
        Y = dbscan(X, metric='precomputed', eps=20, min_samples=10)
        labels, clusters = Y
        count_dict = {}
        for c in clusters:
            if c in count_dict:
                count_dict[c] += 1
            else:
                count_dict[c] = 1
        count_dict = dict(sorted(count_dict.items(), key=lambda item: item[1]))
        print(len(count_dict))
        print(count_dict)

        # plt.figure(figsize=(12, 9))
        # plt.annotate('Reaction clustering', xy=(0.03, 0.03), xycoords='axes fraction')
        # plt.scatter(X[:, 0], X[:, 1], c=clusters, s=50, cmap='Dark2')
        # plt.show()


if __name__ == "__main__":
    text_cluster = TextClustering()
    text_cluster.cluster_by_precomputed_matrix()
