import torch
from torch import nn

# y = torch.tensor([[1], [2], [3], [4]])
#
# print(y)
#
# y = torch.repeat_interleave(y, torch.tensor([5]), dim=0)
#
# print(y.squeeze(1))
from torch.autograd.grad_mode import F
from torch.nn.functional import one_hot

# y = torch.Tensor([6.4588, 0.4872, 0.4067, 5.8364, 0.4412, 7.5244, 0.4528, 0.6710])
# print(y)
#
# y = y.repeat(5).reshape(5, -1)
# print(y)
#
# y = torch.transpose(y, 0, 1)
#
# print(y)
x = torch.Tensor([[0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 1, 0, 0],
        [0, 1, 0, 0],
        [0, 1, 0, 0],
        [0, 1, 0, 0],
        [0, 1, 0, 0],
        [0, 0, 0, 1],
        [0, 0, 0, 1],
        [0, 0, 0, 1],
        [0, 0, 0, 1],
        [0, 0, 0, 1],
        [0, 0, 1, 0],
        [0, 0, 1, 0],
        [0, 0, 1, 0],
        [0, 0, 1, 0],
        [0, 0, 1, 0]])

print(x)

x = torch.sum(x,dim=1)
print(x)

x = x.reshape(-1, 5)
print(x)