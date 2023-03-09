import torch.nn
import py4ml.utils.util_pytorch as pytorch_utils
import torch
import torch.utils.data
import py4ml.utils.params
import py4ml.utils.util_lightning as util_lightning
import logging

class DatasetForMLP(torch.utils.data.Dataset):

    def __init__(self, df, column_x, column_y, transformer= None):
        #
        # df - dataset pandas dataframe
        # column_x - column header / identifier / function for getting x
        # column_y - column header / identifier / function for getting y
        super().__init__()
        self.df = df
        self.column_x = column_x
        self.column_y = column_y
        self.transformer = transformer

    def __getitem__(self, index):
        #
        # index - row index in the dataframe
        x = self.df.iloc[index, [self.df.columns.get_loc(c) for c in self.column_x]]
        y = self.df.iloc[index, [self.df.columns.get_loc(c) for c in self.column_y]]

        # gpu / cpu training
        device = py4ml.utils.params.cfg[py4ml.utils.params.PYTORCH_DEVICE]
        xtensor = torch.as_tensor(x, dtype = torch.float32, device = device) # type: ignore
        ytensor = torch.as_tensor(y, dtype=torch.float32, device = device) # type: ignore

        return [xtensor, ytensor]

    def __len__(self):
        return len(self.df)

def get_dataloaders(train, val, test, batch_size, column_x, column_y, transformer = None):
    train_dl = None
    if train is not None:
        train_ds = DatasetForMLP(train, column_x, column_y, transformer = transformer)
        train_dl = torch.utils.data.DataLoader(train_ds, batch_size, shuffle=True)
    val_dl = None
    if val is not None:
        val_ds = DatasetForMLP(val, column_x, column_y, transformer = transformer)
        val_dl = torch.utils.data.DataLoader(val_ds, batch_size, shuffle=False)
    test_dl = None
    if test is not None:
        test_ds = DatasetForMLP(test, column_x, column_y, transformer = transformer)
        test_dl = torch.utils.data.DataLoader(test_ds, batch_size, shuffle=False)

    batch_func = (lambda dl : len(dl) if dl else 0)
    batch_numbers = list(map(batch_func, [train_dl, val_dl, test_dl]))
    logging.info('batch numbers - train val test=%s', batch_numbers)

    return train_dl, val_dl, test_dl

class MlpWithLightning(util_lightning.LightningModelWrapper):

    def __init__(self, mlp_units, mlp_dropouts, transformer, optimizer):
        super().__init__(optimizer, transformer)
        logging.info('initializing %s', locals())

        self.save_hyperparameters()

        self.model = pytorch_utils.create_mlp(mlp_units, mlp_dropouts)

    def forward(self, x):
        #x = x.view(x.size(0), -1) #.to(DEVICE)
        output = self.model(x)
        output = output.view(output.size(0), -1)
        return output