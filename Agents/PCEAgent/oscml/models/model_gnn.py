import collections
import logging

import dgl
import networkx
import numpy as np
import pytorch_lightning as pl
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
import rdkit.Chem.rdmolops
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.utils.data

import oscml.utils.params
import oscml.utils.util
from oscml.utils.util import smiles2mol
import oscml.utils.util_pytorch
import oscml.utils.util_lightning

class Mol2seq_simple():

    def __init__(self, node2index={}, fix=False, oov=False):

        if fix:
            self.node2index = dict(node2index)
        else:
            self.node2index = collections.defaultdict(lambda:len(self.node2index), node2index)

        self.oov = oov
        # node index starts with 0, thus -1
        self.max_index = len(self.node2index) - 1
        logging.info('initialized Mol2seq_simple with fix=%s, oov=%s, max_index=%s', fix, oov, self.max_index)

    def apply_OOV(self, index):
        return index if index <= self.max_index else -1

    def __call__(self, m):
        seq = []
        for a in m.GetAtoms():
            node = (a.GetSymbol(), a.GetIsAromatic())
            try:
                index = self.node2index[node]
                if self.oov:
                    index = self.apply_OOV(index)
            except KeyError as keyerror:
                if self.oov:
                    index = -1
                else:
                    raise keyerror

            seq.append(index)
        return seq

class DatasetForGnnWithTransformer(torch.utils.data.Dataset):

    def __init__(self, df, mol2seq_fct, smiles_fct, target_fct):
        super().__init__()

        self.df = df
        # sequence of atom types to sequence of atom types indices
        # C, Caromatic, H, C -> 0,1,2,0
        self.mol2seq_fct = mol2seq_fct

        if isinstance(smiles_fct, str):
            self.smiles_fct = lambda data: data[smiles_fct]
        else:
            self.smiles_fct = smiles_fct
        if isinstance(target_fct, str):
            self.target_fct = lambda data: data[target_fct]
        else:
            self.target_fct = target_fct

        # this is to speed up the processing
        self.smiles2seq = dict()

    def __getitem__(self, index):

        row = self.df.iloc[index]
        smiles = self.smiles_fct(row)

        value = self.smiles2seq.get(smiles)
        """
        if value:
            m = value[0]
            seq = value[1]
        else:
            m = smiles2mol(smiles)
            seq = self.mol2seq_fct(m)
            self.smiles2seq[smiles] = (m, seq)
        """
        if value:
            seq = value[0]
            g = value[1]
        else:
            # smiles string -> molecular graph
            m = smiles2mol(smiles)
            # molecular graph -> sequence of indices of atom types
            seq = self.mol2seq_fct(m)
            #    1 2 3   - this is to get the connectivity
            # 1  0 1 0   - rows must have the same ordering of atoms!
            # 2  1 0 0
            # 3 ...
            adj = rdkit.Chem.rdmolops.GetAdjacencyMatrix(m)
            # so this an intermediate step, from adj -> g_nx
            g_nx = networkx.convert_matrix.from_numpy_matrix(adj)
            # g_nx -> g
            g = dgl.from_networkx(g_nx)
            # cache this info to speed training
            self.smiles2seq[smiles] = (seq, g)

        #adj = rdkit.Chem.rdmolops.GetAdjacencyMatrix(m)
        # from 3 to 6 seconds
        #g_nx = networkx.convert_matrix.from_numpy_matrix(adj)
        # 28 second, second run 25
        #g = dgl.from_networkx(g_nx)
        # 0,1,2... numpy array into tensor
        tensor = torch.as_tensor(seq, dtype=torch.long)
        # define 'type' var in the graph
        g.ndata['type'] = tensor

        y = self.target_fct(row)
        y = torch.as_tensor(np.array(y, dtype=np.float32))
        return [g, y]

    def __len__(self):
        return len(self.df)

def collate_fn(data):
    # collate function to transform output from the dataloader
    # this is custom batching of graphs from dgl
    # takes batch of graphs and then create one big dgl graph to be more efficient
    graphs, y = map(list, zip(*data))
    g_batch = dgl.batch(graphs)
    y_batch = torch.as_tensor(y)
    return [g_batch, y_batch]

def get_dataloaders_internal(train, val, test, batch_size, mol2seq, transformer):

    smiles_fct = transformer.transform_x
    target_fct = transformer.transform

    train_dl = None
    if train is not None:
        train_ds = DatasetForGnnWithTransformer(train, mol2seq, smiles_fct, target_fct)
        train_dl = torch.utils.data.DataLoader(train_ds, batch_size, shuffle=True, collate_fn=collate_fn)
    val_dl = None
    if val is not None:
        val_ds = DatasetForGnnWithTransformer(val, mol2seq, smiles_fct, target_fct)
        val_dl = torch.utils.data.DataLoader(val_ds, batch_size, shuffle=False, collate_fn=collate_fn)
    test_dl = None
    if test is not None:
        test_ds = DatasetForGnnWithTransformer(test, mol2seq, smiles_fct, target_fct)
        test_dl = torch.utils.data.DataLoader(test_ds, batch_size, shuffle=False, collate_fn=collate_fn)

    batch_func = (lambda dl : len(dl) if dl else 0)
    batch_numbers = list(map(batch_func, [train_dl, val_dl, test_dl]))
    logging.info('batch numbers - train val test=%s', batch_numbers)

    return train_dl, val_dl, test_dl

def get_dataloaders(dataset, df_train, df_val, df_test, transformer, batch_size):

    info = oscml.data.dataset.get_dataset_info(dataset)
    node2index = info.node_types
    mol2seq = oscml.models.model_gnn.Mol2seq_simple(node2index, fix=True, oov=True)
    return get_dataloaders_internal(df_train, df_val, df_test, batch_size, mol2seq, transformer)

class SimpleGNNLayer(pl.LightningModule):

    def __init__(self, input_dim, output_dim, activation_fct):
        super().__init__()
        # weights matrix A^(1,2,...)
        self.linear = nn.Linear(input_dim, output_dim)
        self.activation_fct = activation_fct
        # copy all h vectors of your nodes and declare them as m
        # we take h's of the neighbour modes and re-label them as m to not confuse
        # them with the h of the center node
        self.msg = dgl.function.copy_src(src='h', out='m')
        # then aggregate all m vectors into the center h vector
        self.reduce = dgl.function.mean(msg='m', out='h')

    def forward(self, g, h_input_features):
        with g.local_scope():
            # attach to each node h vectors (list operation! do not use loop because it is expensive)
            g.ndata['h'] = h_input_features # it is a matrix, each row is a node vector to be assigned
            g.update_all(self.msg, self.reduce)
            h = g.ndata['h']   # adding initial h0 ?
            h = self.linear(h) # A weights matrix for each layer
            h = h + h_input_features
            return self.activation_fct(h) # relu

class SimpleGNN(oscml.utils.util_lightning.OscmlModule):
# main class for the SimpleGNN
    def __init__(self, node_type_number, embedding_dim, conv_dims, mlp_units, padding_index, target_mean, target_std, optimizer, mlp_dropouts=None):
        # inputs:
        # node_type_number - 8 atom types (atom types + they aromaticity, equiv to WL wit r=0)
        # embedding_dim    - the same as in BiLSTM ( but you need node_type_number)
        # conv_dims        - defines the dimension of the vectors in layers for collating info about the direct neighbour nodes (A matrix)
        #                    e.g  for conv_dims = [16 8 4] then nr of layers = 3 (~its like r=3 in WL)
        #                    if embedding_dim = 128 then we map 128 -> 16, A^1 (16,8), A^2 (8,4)
        # mlp_units        - list defining the nr of units in each fully connected layer
        # padding_index    - for out of vocabulary case
        # target_mean/std  - used for set transformation, to transform back PCE data and get the errors
        super().__init__(optimizer, target_mean, target_std)
        logging.info('initializing %s', locals())

        # we do not need this anymore, it is for resuming training
        self.save_hyperparameters()

        if padding_index is not None:
            self.padding_index = padding_index
            logging.info('padding index for embedding was set to %s . Thus unknown atom types can be handled.', self.padding_index)
            # consider the padding index for subsequential transfer learning with unknown atom types:
            # we add +1 to node_type_number because
            # padding_idx = 0 in a sequences is mapped to zero vector
            self.embedding = nn.Embedding(node_type_number+1, embedding_dim, padding_idx=self.padding_index)
        else:
            self.padding_index = None
            logging.info('No padding index for embedding was set. No transfer learning for unknown atom types will be possible')
            self.embedding = nn.Embedding(node_type_number, embedding_dim)

        self.conv_modules = nn.ModuleList()
        #so conv_dims comes from config file
        input_dim = embedding_dim
        for i in range(len(conv_dims)):
            layer = SimpleGNNLayer(input_dim, conv_dims[i], F.relu)
            input_dim = conv_dims[i]
            self.conv_modules.append(layer)

        # input_dim after the above loop will be set to the last conv_dims dimension
        mlp_units_with_input_dim = [input_dim]
        mlp_units_with_input_dim.extend(mlp_units)
        self.mlp = oscml.utils.util_pytorch.create_mlp(mlp_units_with_input_dim, mlp_dropouts)
        self.one = torch.Tensor([1]).long()

    def forward(self, graphs):
        if isinstance(graphs, list):
            # each molecule is represented via graph
            # we want to pass and process multiple molecules to speed things up
            # so we can pass N nr of graphs for all molecules in a batch
            # then dgl creates one large graph out of it to parallelize calculations
            # this avoids loops (the graphs are still disjoint from each other)
            # not really used in the code, just for unit testing
            g_batch = dgl.batch(graphs)
        else:
            # super graph for the batch
            # batching is already done in the 'collate_fn'
            g_batch = graphs

        # g_batch.ndata['type'] defined in the dataloader
        # the atoms types are starting from index=0
        # but the index 0 is already reserved for out of vocabulary
        # sequence is important here to assign correct h^0 vectors to the correct graph nodes
        seq_types = g_batch.ndata['type']
        # seq_types = [0,1,2,1,0...
        #              1,0,1,2      ]
        if self.padding_index is not None:
            # minus_one should be called plus_one
            minus_one = self.one.to(self.device).expand(seq_types.size())
            seq_types = seq_types + minus_one

        # h = [ 0  - out of voc vector
        #       1  - for atom type 0 vector
        #       ....]
        h = self.embedding(seq_types)
        for layer in self.conv_modules:
            h = layer(g_batch, h)
        g_batch.ndata['h'] = h # this is h^L, and the order is always respected

        mean = dgl.mean_nodes(g_batch, 'h')
        o = self.mlp(mean)
        # transform from size [batch_number, 1] to [batch_number]
        o = o.view(len(o))
        return o
