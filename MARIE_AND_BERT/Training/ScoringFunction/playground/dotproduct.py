import torch

x = torch.rand(4)
y = torch.rand(4)

product = torch.sum(x * y)
print(product)


x = torch.rand(4)
print(x)
x = x.repeat(10, 1)
print(x)