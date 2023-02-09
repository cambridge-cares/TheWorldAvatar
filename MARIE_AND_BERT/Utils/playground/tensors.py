import os

import pandas as pd
import torch
from torch import nn
#
# # y = torch.tensor([[1], [2], [3], [4]])
# #
# # print(y)
# #
# # y = torch.repeat_interleave(y, torch.tensor([5]), dim=0)
# #
# # print(y.squeeze(1))
# from torch.autograd.grad_mode import F
# from torch.nn.functional import one_hot, normalize
#
# # y = torch.Tensor([6.4588, 0.4872, 0.4067, 5.8364, 0.4412, 7.5244, 0.4528, 0.6710])
# # print(y)
# #
# # y = y.repeat(5).reshape(5, -1)
# # print(y)
# #
# # y = torch.transpose(y, 0, 1)
# #
# # print(y)
# from Marie.Util.location import DATA_DIR
#
# x = torch.Tensor([[0, 0, 0, 0],
#                   [0, 0, 0, 0],
#                   [0, 0, 0, 0],
#                   [0, 0, 0, 0],
#                   [0, 0, 0, 0],
#                   [0, 1, 0, 0],
#                   [0, 1, 0, 0],
#                   [0, 1, 0, 0],
#                   [0, 1, 0, 0],
#                   [0, 1, 0, 0],
#                   [0, 0, 0, 1],
#                   [0, 0, 0, 1],
#                   [0, 0, 0, 1],
#                   [0, 0, 0, 1],
#                   [0, 0, 0, 1],
#                   [0, 0, 1, 0],
#                   [0, 0, 1, 0],
#                   [0, 0, 1, 0],
#                   [0, 0, 1, 0],
#                   [0, 0, 1, 0]])
#
# print(x)
#
# x = torch.sum(x, dim=1)
# print(x)
#
# x = x.reshape(-1, 5)
# print(x)
#
# X = torch.rand(84, 40)
# print(X)
# print(normalize(X, dim=0))
#
# attr_embedding = pd.read_csv(os.path.join(DATA_DIR, "CrossGraph/wikidata_numerical", 'attr_embedding.tsv'), sep='\t',
#                              header=None)
#
# attr_tensor = torch.tensor(attr_embedding.values)
# print(attr_tensor.shape)
# x = X[0].repeat(258, 1)
# print(x.shape)
# print(x)
# other_attr = X[0]
# cosine_similarity = torch.nn.CosineSimilarity(dim=1, eps=1e-08)
#
# cos_sim = (cosine_similarity(attr_tensor, x))
# good_idx = (torch.argmax(cos_sim))
# print(cos_sim.shape)
# good_attr = attr_tensor[good_idx]
# print(good_attr)
# print(other_attr)
#
# labels = torch.randint(4, [100, 1])
# print(labels)
# encoded_labels = one_hot(labels, num_classes=4).to(torch.float)
# print(encoded_labels)
#
# X = torch.rand(10, 4)
# print(X)
# splitted_X = torch.tensor_split(X, [1, 9, 10])
# print(splitted_X)
# print(splitted_X)
#
# x = [0, 1, 2, 3, 4, 5, 6, 7]
#
# print(x[0:5])

A = torch.LongTensor(range(10))
B = torch.LongTensor(range(10))
C = torch.LongTensor(range(10))
print(A)
print(B)
print(C)
idx = (C > 2)
print(idx)
A = A[idx]
print(A)