from leven import levenshtein
import numpy as np
from sklearn.cluster import dbscan

data = ["abc", "abcd", "efgh", "efgh2", "reactions", "something else", "abcdef"]

data = data * 100


def lev_metric(x, y):
    i, j = int(x[0]), int(y[0])  # extract indices
    return levenshtein(data[i], data[j])


X = np.arange(len(data)).reshape(-1, 1)
print(X)
rst = dbscan(X, metric=lev_metric, eps=2, min_samples=1)
print(rst)
