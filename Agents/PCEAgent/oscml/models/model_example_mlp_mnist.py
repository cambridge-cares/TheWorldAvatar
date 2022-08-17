import os

import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import torch.utils.data
from torchvision import datasets
from torchvision import transforms

import pytorch_lightning as pl

from oscml.utils.util import log


"""
code was extracted from https://github.com/optuna/optuna/blob/master/examples/pytorch_simple.py
and https://github.com/optuna/optuna/blob/master/examples/pytorch_lightning_simple.py
"""

def get_mnist(mnist_dir, batch_size):

    train_loader = torch.utils.data.DataLoader(
        datasets.MNIST(mnist_dir, train=True, download=True, transform=transforms.ToTensor()),
        batch_size=batch_size,
        shuffle=True,
    )
    valid_loader = torch.utils.data.DataLoader(
        datasets.MNIST(mnist_dir, train=False, transform=transforms.ToTensor()),
        batch_size=batch_size,
        shuffle=False,
    )

    return train_loader, valid_loader

class MlpWithLightning(pl.LightningModule):
    
    def __init__(self, number_classes, layers, units, dropouts, optimizer, optimizer_lr):
        super().__init__()
        self.save_hyperparameters()
        self.optimizer = optimizer
        self.optimizer_lr = optimizer_lr
       
        layer_list = []
        in_features = 28 * 28
        for i in range(layers):
            out_features = units[i]
            layer_list.append(nn.Linear(in_features, out_features))
            layer_list.append(nn.ReLU())
            dropout = dropouts[i]
            layer_list.append(nn.Dropout(dropout))
            in_features = out_features
        layer_list.append(nn.Linear(in_features, number_classes))
        layer_list.append(nn.LogSoftmax(dim=1))

        self.model = nn.Sequential(*layer_list)
        
    def forward(self, x):
        x = x.view(x.size(0), -1) #.to(DEVICE)
        output = self.model(x)
        output = output.view(output.size(0), -1)
        return output
        
    def configure_optimizers(self):
        optimizer = getattr(optim, self.optimizer)(self.parameters(), lr=self.optimizer_lr)
        return optimizer
        
    def training_step(self, batch, batch_nb):
        data, target = batch
        output = self.forward(data)
        loss = F.nll_loss(output, target)
        self.log('loss', loss) #, on_step=True, on_epoch=True, prog_bar=True)
        return loss
    
    def validation_step(self, batch, batch_nb):
        data, target = batch
        output = self.forward(data)
        pred = output.argmax(dim=1, keepdim=True)
        accuracy = pred.eq(target.view_as(pred)).float().mean()
        return accuracy

    def validation_epoch_end(self, outputs):
        accuracy = sum(x for x in outputs) / len(outputs)
        log('\nvalidation_epoch_end val_acc=', accuracy)
        self.log('val_acc', accuracy)