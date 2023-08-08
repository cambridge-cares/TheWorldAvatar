import torch
from torch import nn
t1 = torch.tensor([[1,2,3,4],[1,2,3,0]])
t2 = torch.tensor([[0,1,2,3],[1,2,3,4]])

b = torch.tensor([[10]])
print(t1 * t2)
av = torch.sum(t1 * t2, dim=1)
print(av + b)
print(av + b[0])

emb1 = nn.Embedding(num_embeddings=5, embedding_dim=5)
emb1.weight.data.uniform_(-5, 5)
x = emb1(torch.tensor(1))
print(x)
print(torch.mean(x))
y = nn.Embedding(embedding_dim=5, num_embeddings=5 + 1, padding_idx=5)
print(y)