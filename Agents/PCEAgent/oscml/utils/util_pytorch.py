import datetime
import os
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
import torch.utils.data
from oscml.utils.params import cfg
import oscml.utils.util
from oscml.utils.util import log
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

def fit(epochs, model, loss_func, optimizer, train_dl, val_dl,
        inverse_transform_func, tensorboard_logger=None):

    log('model=', type(model))
    log('train batch number=', len(train_dl), type(train_dl))
    log('val batch number=', len(val_dl), type(val_dl))
    if tensorboard_logger:
        train_tb = tensorboard_logger[0]
        val_tb = tensorboard_logger[1]
    else:
        train_tb = None
        val_tb = None
    log('tensorboard logger=', str(train_tb),  str(val_tb))

    total_batch_number = 0
    for epoch in range(1, epochs+1):

        log('epoch', epoch, '/', epochs, 'training')

        model.train()

        batch_number = 0
        for x_batch, y_batch in train_dl:
            batch_number += 1
            total_batch_number += 1
            optimizer.zero_grad()
            output_batch = model(x_batch)
            loss = loss_func(output_batch, y_batch)
            loss.backward()
            optimizer.step()

            with torch.no_grad():
                o_inv = inverse_transform_func(output_batch)
                y_inv = inverse_transform_func(y_batch)
                mse = loss_func(o_inv, y_inv)
                mae = nn.L1Loss()(o_inv, y_inv)
                log('train', epoch, batch_number, len(y_batch),
                    {'mse': mse.item(), 'mae': mae.item(), 'loss_z': loss.item()})
                if train_tb:
                    train_tb.add_scalar('mse', mse.item(), total_batch_number)
                    train_tb.add_scalar('mae', mae.item(), total_batch_number)

        log('epoch', epoch, '/', epochs, 'validation')

        _, _, metrics = predict(model, loss_func, val_dl, 'val', inverse_transform_func, epoch)
        if val_tb:
            val_tb.add_scalar('mse', metrics['mse'], total_batch_number)
            val_tb.add_scalar('mae', metrics['mae'], total_batch_number)

    if tensorboard_logger:
        train_tb.close()
        val_tb.close()

def predict(model, loss_func, dl, dl_name, inverse_transform_func, epoch=None):

    log('predict on sample set=', dl_name)

    model.eval()

    with torch.no_grad():
        device = cfg[oscml.utils.params.PYTORCH_DEVICE]
        o_inv_all = torch.FloatTensor([]).to(device)
        y_inv_all = torch.FloatTensor([]).to(device)
        batch_number = 0

        for x_batch, y_batch in dl:
            batch_number += 1
            output_batch = model(x_batch)
            o_inv = inverse_transform_func(output_batch)
            y_inv = inverse_transform_func(y_batch)
            o_inv_all = torch.cat([o_inv_all, o_inv])
            y_inv_all = torch.cat([y_inv_all, y_inv])
            log(batch_number, '/', len(dl))

        mse = loss_func(o_inv_all, y_inv_all)
        mae = nn.L1Loss()(o_inv_all, y_inv_all)

        y = y_inv_all.cpu().numpy()
        y_pred = o_inv_all.cpu().numpy()
        metrics = oscml.utils.util.calculate_metrics(y, y_pred)

        metrics_dict = {'mse': mse.item(),
                        'mae': mae.item(),
                        'R2': metrics['R2'],
                        'r': metrics['r']}

    log(dl_name, epoch, metrics_dict)

    return y, y_pred, metrics_dict

def save(epoch, model, optimizer, postfix=''):
    filename = datetime.datetime.now().strftime('%y%m%d_%H%M') \
            + '_e' + str(epoch) + postfix + '.pth'
    path = './pth'
    if not os.path.exists(path):
        os.makedirs(path)

    torch.save({
            'epoch': epoch,
            'model_state_dict': model.state_dict(),
            'optimizer_state_dict': optimizer.state_dict()
            }, path + '/' + filename)

def load(model, optimizer, filename, filepath = './pth', cpu=False):
    if cpu:
        checkpoint = torch.load(filepath + '/' + filename, map_location=torch.device('cpu'))
    else:
        checkpoint = torch.load(filepath + '/' + filename)
    model.load_state_dict(checkpoint['model_state_dict'])
    optimizer.load_state_dict(checkpoint['optimizer_state_dict'])
    #epoch = checkpoint['epoch']

    model.eval()

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