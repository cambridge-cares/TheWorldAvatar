import torch
# from torch import nn, no_grad, unsqueeze

loss = torch.nn.CrossEntropyLoss()


input = torch.randn(3, 5, requires_grad=True)
target = torch.empty(3, dtype=torch.long).random_(5)

print(input)
print(target)
output = loss(input, target)
output.backward()

loss = torch.nn.MultiLabelSoftMarginLoss()
# Example of target with class probabilities
input = torch.randn(3, 5, requires_grad=True)
target = torch.randn(3, 5).softmax(dim=1)
output = loss(input, target)
output.backward()

print(input)
print(target)