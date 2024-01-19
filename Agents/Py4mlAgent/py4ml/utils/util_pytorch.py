import os
import torch
import torch.nn as nn
import torch.utils.data
import numpy as np
import random

def torch_init(seed=None,cudnn_deterministic=None,cudnn_benchmark=None):
    seed = int(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False
    np.random.seed(seed)
    random.seed(seed)
    os.environ['PYTHONHASHSEED'] = str(seed)

def create_mlp(mlp_dim_list, dropout_list=None):
    number_mlp_layers = len(mlp_dim_list) - 1
    mlp_modules = []
    for i in range(number_mlp_layers):
        mlp_modules.append(nn.Linear(mlp_dim_list[i], mlp_dim_list[i+1]))
        # no ReLU for the last layer
        if i < number_mlp_layers - 1:
            mlp_modules.append(nn.ReLU())
            if dropout_list:
                dropout = dropout_list[i]
                if dropout > 0:
                    mlp_modules.append(nn.Dropout(dropout))
    return nn.Sequential(*mlp_modules)