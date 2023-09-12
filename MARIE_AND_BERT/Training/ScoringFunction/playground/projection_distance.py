#
import torch
from torch.nn.init import xavier_uniform

t1 = torch.rand(2, 3)
t1 = xavier_uniform(t1)

t2 = torch.rand(2, 3)
t2 = xavier_uniform(t2)
print(t1)
print(t2)

def distance(t1, t2):

    return torch.norm(t1 - t2, dim= 1, p=1)

print(distance(t1, t2))