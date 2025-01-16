import json
import math
import os
import pickle
import random
from itertools import product

import numpy as np
from matplotlib import pyplot as plt
from sklearn.cluster import dbscan
import sys

sys.path.append("../..")
from Marie.Util.location import DATA_DIR


def over_lapping_metric(i, j, reaction_species_dict):
    s_list_1 = set(reaction_species_dict[i])
    s_list_2 = set(reaction_species_dict[j])
    l_1 = len(s_list_1)
    l_2 = len(s_list_2)
    distance = round(max(len(s_list_1 ^ s_list_2), 0) / (l_1 + l_2), 5) * 10
    return distance


class TextClustering:

    def __init__(self):
        self.dataset_dir = "CrossGraph/ontokin_reactions"
        self.ontology = "ontokin_reactions"
        self.full_dataset_dir = os.path.join(DATA_DIR, self.dataset_dir)
        self.result_dict = {}

    def run(self):
        pass

    def make_distance_matrix(self, selected_reaction_idx_list, reaction_species_dict):
        matrix = []
        counter = 0
        list_length = len(selected_reaction_idx_list)
        for i in selected_reaction_idx_list:
            counter += 1
            progress = round(counter / list_length * 100, 2)
            print(f"{progress} %")
            row = []
            for j in selected_reaction_idx_list:
                d = over_lapping_metric(i, j, reaction_species_dict=reaction_species_dict)
                row.append(d)
            matrix.append(row)

        return matrix

    def scan_cluster(self, distance_matrix):
        eps_list = np.arange(4, 6, 0.1)
        min_sample_list = np.arange(8, 15, 1)
        for eps, min_sample in product(eps_list, min_sample_list):
            eps = round(eps, 1)
            self.cluster_by_precomputed_matrix(distance_matrix, eps=eps, min_samples=min_sample)
        return self.result_dict

    def cluster_by_precomputed_matrix(self, distance_matrix, eps=1.0, min_samples=3):
        X = distance_matrix
        X = np.array(X)
        Y = dbscan(X, metric='precomputed', eps=eps, min_samples=min_samples)
        labels, clusters = Y
        count_dict = {}
        for c in clusters.tolist():
            if c in count_dict:
                count_dict[c] += 1
            else:
                count_dict[c] = 1
        count_dict = dict(sorted(count_dict.items(), key=lambda item: item[1]))
        # print(len(count_dict))
        print("---------")
        print(f"eps: {eps}")
        print(f"min_samples: {min_samples}")
        print("---------")
        print(count_dict)
        max_value = np.amax(X)
        min_value = np.amin(X)

        result_key = f"{eps}_{min_samples}"
        result = {"reaction_idx_list": clusters.tolist(), "count_dict": count_dict}
        self.result_dict[result_key] = result

        print("=========================================================")

        # print(f"max value {max_value}")
        # print(f"min value {min_value}")

        # plt.figure(figsize=(12, 9))
        # plt.annotate('Reaction clustering', xy=(0.03, 0.03), xycoords='axes fraction')
        # plt.scatter(X[:, 0], X[:, 1], c=clusters, s=50, cmap='Dark2')
        # plt.show()
        # return self.result_dict

# if __name__ == "__main__":
#     text_cluster = TextClustering()
#     eps_list = np.arange(4, 6, 0.1)
#     min_sample_list = np.arange(8, 15, 1)
#     for eps, min_sample in product(eps_list, min_sample_list):
#         eps = round(eps, 1)
#         text_cluster.cluster_by_precomputed_matrix(eps=eps, min_samples=min_sample)
#     # eps = 4.9
#     # min_sample = 8
#     # text_cluster.cluster_by_precomputed_matrix(eps=eps, min_samples=min_sample)
#
#     with open(os.path.join(text_cluster.full_dataset_dir, "cluster_result.json"), "w") as f:
#         f.write(json.dumps(text_cluster.result_dict))
#         f.close()
