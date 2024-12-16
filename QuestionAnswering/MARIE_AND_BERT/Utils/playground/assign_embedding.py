import torch
from torch import nn, no_grad

head_embedding = nn.Embedding(5, 8)
rel_embedding = nn.Embedding(5, 8)
tail_embedding = nn.Embedding(5, 8)
all_idx = torch.tensor([0, 1, 2, 3])

calculated_embedding = head_embedding(all_idx) + rel_embedding(all_idx)
print(calculated_embedding)
with no_grad():
    tail_embedding.weight[all_idx] = calculated_embedding
    print(tail_embedding.weight)
    distance = (head_embedding(all_idx) + rel_embedding(all_idx) - tail_embedding(all_idx)).norm(p=2, dim=-1)
    print(distance)

proj_matrix = torch.rand(5, 5)

print(proj_matrix)
print(proj_matrix.inverse())

X = head_embedding.weight
print(X)
print(X.view(-1, 20, 1))

# print(embedding.weight)
# print(embedding.weight[0])
# zeros = torch.zeros(8)
# ones = torch.ones(8)
# print(zeros)
# print(ones)
# with no_grad():
#     embedding.weight[[0, 2]] = torch.stack([ones, zeros])
# print(embedding.weight)
