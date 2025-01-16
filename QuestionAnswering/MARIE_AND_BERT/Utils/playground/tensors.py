import os
import time

import pandas as pd
import torch
from torch import nn, tensor

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

# A = torch.LongTensor(range(10))
# B = torch.LongTensor(range(10))
# C = torch.LongTensor(range(10))
# print(A)
# print(B)
# print(C)
# idx = (C > 2)
# print(idx)
# A = A[idx]
# print(A)
#
# A = tensor([[666, 6, 793, -999],
#             [52, 12, 1115, -999],
#             [1504, 19, 1709, -999],
#             [861, 15, 3872, -999],
#             [665, 18, 1717, -999],
#             [667, 6, 802, -999],
#             [789, 15, 4031, -999],
#             [728, 18, 1784, -999],
#             [629, 6, 983, -999],
#             [660, 18, 1629, -999],
#             [22, 12, 2855, -999],
#             [239, 9, 4294, -999],
#             [150, 8, 1375, -999],
#             [1686, 20, 2683, -999],
#             [1001, 15, 3830, -999],
#             [1427, 19, 1768, -999]])
#
# A = torch.transpose(A, 0, 1)
# # A = A.view(4, -1)
# print(A)

# A =  tensor([[40790.0000,     0.0000,  2266.0000,   379.1500]])
# print(A)
# print(len(A))
# print(len(A[0]))
# A2 = A.repeat(1, 1)
# print(A2)

# x = torch.arange(10)
# s_x = torch.tensor_split(x, (1, 6))
# print(s_x)

# x = torch.randn(10, 5)
# y = torch.randn(10, 5)
#
# print(x)
# print(y)
# r = torch.cat((x, y), 1)
# print(r)

# assume x is the head, y is the tail
# x = [0, 1, 2, 3, 4, 5, 6, 7]
# x = torch.IntTensor(x)
# y = [8, 9, 10]
# y = torch.IntTensor(y)
#
# print(x, y)
# y_repeat_num = len(x)
# x_repeat_num = len(y)
# # repeat each element in x to len(y)
# x = torch.repeat_interleave(x, len(y))
# print(x)
#
# # y repeat to len(x)
# y = y.repeat(y_repeat_num)
# print(y)
# print(x.shape, y.shape)
# split_indices = [indice for indice in range(x_repeat_num, len(y), x_repeat_num)]
# print(split_indices)
#
# print(torch.tensor_split(y, split_indices))
# print(len(torch.tensor_split(y, split_indices)))
# from torch.nn.functional import one_hot
#
# domains = torch.tensor([1, 3, 4])
# encoded = one_hot(domains, num_classes=5)
# print(encoded)
# print(torch.sum(encoded, dim=0))
test_cos = torch.nn.CosineSimilarity(dim=0, eps=1e-08)

x = torch.LongTensor([[40] * 40] * 10)
print(x)

y = torch.ones(40)
print(y)

sim = test_cos(x,y)
print(sim)
#
# sim = (x - y).norm(p=1, dim=0)
# print(sim)
#
# C = torch.randn(10)
# print(C)
#
# A, B = torch.topk(C, k=len(C), largest=False)
#
# print(A)
# print(B)
#
#
#
# Y = torch.LongTensor([1, 2, 3, 4])
# print(Y)

# X = [[tensor([2431]), tensor([89]), tensor([3867]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([5446]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([10689]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([10930]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([10960]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([12481]), tensor([5452])],
#      [tensor([2431]), tensor([89]), tensor([5452]), tensor([5452])]]
#
# X = torch.LongTensor(X)
# print(X)
# print(X.transpose(0,1))
# X = X.transpose(0,1)
# heads, rels, all_tails, true_tail = X[0], X[1], X[2], X[3][0].item()
# print(heads, rels, all_tails, true_tail)
#
# # print(torch.stack(X))
# A = torch.LongTensor([1])
# A = A.repeat(10)
# X = torch.range(0, 9)
#
# print(A)
# print(X)
#
# Y = torch.LongTensor([1, 2, 3, 4])
#
# START_TIME = time.time()
# X = X[~Y.unsqueeze(1).eq(X).any(0)].long()
# print(time.time() - START_TIME)
# print(X)
