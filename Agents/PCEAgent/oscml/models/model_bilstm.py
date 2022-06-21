import collections
import logging
import numpy as np
import pytorch_lightning as pl
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
import torch
import torch.nn as nn
import torch.utils.data
import oscml.utils.params
from oscml.utils.params import cfg
from oscml.utils.util import smiles2mol, concat
import oscml.utils.util_lightning as util_lightning
import oscml.utils.util_pytorch
import oscml.features.weisfeilerlehman


class Mol2seq():

    def __init__(self, radius, oov, wf=None):
        # radius - WL radius
        # oov - out of vocabulary flag
        # wf - lookup dictionaries
        self.radius = radius
        self.oov = oov
        self.wf = wf

        if wf:
            atom_dict = wf['atom_dict']
            bond_dict = wf['bond_dict']
            fragment_dict = wf['fragment_dict']
            edge_dict = wf['edge_dict']

        self.atom_dict = collections.defaultdict(lambda:len(self.atom_dict), atom_dict)
        self.bond_dict = collections.defaultdict(lambda:len(self.bond_dict), bond_dict)
        self.fragment_dict = collections.defaultdict(lambda:len(self.fragment_dict), fragment_dict)
        self.edge_dict = collections.defaultdict(lambda: len(self.edge_dict), edge_dict)

        # fragment index starts with 0, thus -1
        self.max_index = len(self.fragment_dict) - 1
        logging.info('initialized Mol2Seq with radius=%s, oov=%s, max_index=%s', radius, oov, self.max_index)

    def apply_OOV(self, index):
        # oov fragment index will be set to -1 so that when the +1 padding is applied
        # this index will be set to 0 (-1+1)
        # be careful to not change the  padding index, if you do, you need to also change the
        # hardcoded "-1"
        return (index if index <= self.max_index else -1)

    def __call__(self, m):
        # inputs:
        # m - rdkit mol structure of a species
        #
        # this transforms m into a fragments sequence in BFS order (not padded yet)

        # molecule fragmentation via WL algorithm
        atoms, i_jbond_dict = oscml.features.weisfeilerlehman.get_atoms_and_bonds(m, self.atom_dict, self.bond_dict)
        descriptor = oscml.features.weisfeilerlehman.extract_fragments(self.radius, atoms, i_jbond_dict, self.fragment_dict, self.edge_dict)
        atoms_BFS_order = oscml.features.weisfeilerlehman.get_atoms_BFS(m)
        if self.oov:
            descriptor_BFS = [self.apply_OOV(descriptor[i]) for i in atoms_BFS_order]
        else:
            descriptor_BFS = [descriptor[i] for i in atoms_BFS_order]
        return descriptor_BFS

class DatasetForBiLstmWithTransformer(torch.utils.data.Dataset):

    def __init__(self, df, max_sequence_length, m2seq_fct, padding_index, smiles_fct, target_fct):
        #
        # df - dataset pandas dataframe
        # max_sequence_length - max sequence length from the config file - it sets the attention layer size
        # m2seq_fct - is a call method of Mol2seq class with default parameters set for the requested dataset type (CEPDB/HOPV15)
        # padding_index - hardcoded padding index (0)
        # smiles_fct - column header / identifier / function for getting smiles string out of pandas dataframe
        # target_fct - column header / identifier / function for getting pce out of pandas dataframe
        super().__init__()
        self.df = df
        self.max_sequence_length = max_sequence_length
        self.m2seq_fct = m2seq_fct
        # TODO: use torch.nn.utils.rnn.pack_padded_sequence instead
        # padding_sequence gives [0, 0, ... up to max_sequence_length]
        self.padding_sequence = [padding_index]*self.max_sequence_length

        if isinstance(smiles_fct, str):
            self.smiles_fct = lambda data: data[smiles_fct]
        else:
            self.smiles_fct = smiles_fct
        if isinstance(target_fct, str):
            self.target_fct = lambda data: data[target_fct]
        else:
            self.target_fct = target_fct

        self.smiles2seq = dict()

    def __getitem__(self, index):
        #
        # index - row index in the dataframe
        row = self.df.iloc[index]
        smiles = self.smiles_fct(row)

        # converve memory,e.g. if a given smiles (molecule) has been already transformed, just
        # look it up and use it, if not then proceed to the next step
        x = self.smiles2seq.get(smiles)

        if x is None:
            # smiles to rdkit mol object
            m = smiles2mol(smiles)
            # m to fragments sequence vector in BFS (not padded yet) but it may include oov indices as "-1"
            x = self.m2seq_fct(m)
            # increase all indexes by +1 - this this would shift any oov indices to "0"
            x = np.array(x) + 1
            # fill up the sequence with padding index 0
            diff = self.max_sequence_length-len(x)
            if diff > 0:
                x = np.append(x, self.padding_sequence[:diff])
                self.smiles2seq[smiles] = x
            if diff < 0:
                raise RuntimeError(concat('A sequence with length greater the maximum sequence length was generated.',
                        ', length=', len(x), ', maximum sequence length=', self.max_sequence_length, ', row index=', str(index)))

        # gpu / cpu training
        device = cfg[oscml.utils.params.PYTORCH_DEVICE]
        x = torch.as_tensor(x, dtype = torch.long, device = device)

        # get the (transformed) pce
        y = self.target_fct(row)
        y = torch.as_tensor(np.array(y, dtype=np.float32), device = device)

        return [x, y]

    def __len__(self):
        return len(self.df)

def get_dataloaders_internal(train, val, test, batch_size, mol2seq, max_sequence_length, smiles_fct, target_fct):

    padding_index = 0

    train_dl = None
    if train is not None:
        train_ds = DatasetForBiLstmWithTransformer(train, max_sequence_length, mol2seq, padding_index, smiles_fct, target_fct)
        train_dl = torch.utils.data.DataLoader(train_ds, batch_size, shuffle=True)
    val_dl = None
    if val is not None:
        val_ds = DatasetForBiLstmWithTransformer(val, max_sequence_length, mol2seq, padding_index, smiles_fct, target_fct)
        val_dl = torch.utils.data.DataLoader(val_ds, batch_size, shuffle=False)
    test_dl = None
    if test is not None:
        test_ds = DatasetForBiLstmWithTransformer(test, max_sequence_length, mol2seq, padding_index, smiles_fct, target_fct)
        test_dl = torch.utils.data.DataLoader(test_ds, batch_size, shuffle=False)

    batch_func = (lambda dl : len(dl) if dl else 0)
    batch_numbers = list(map(batch_func, [train_dl, val_dl, test_dl]))
    logging.info('batch numbers - train val test=%s', batch_numbers)

    return train_dl, val_dl, test_dl


def get_dataloaders(dataset, df_train, df_val, df_test, transformer, batch_size, max_sequence_length):

    # the info objects contains atoms, edges and fragments dictionaries for the requested dataset type (CEPDB or HOPV15)
    # it also contains mol2seq function with those dictionaries set as defaults
    info = oscml.data.dataset.get_dataset_info(dataset)
    # mol2seq => oscml.models.model_bilstm.Mol2seq(radius=1, oov=True, wf=d['wf_r1'])
    mol2seq = info.mol2seq
    return get_dataloaders_internal(df_train, df_val, df_test, batch_size, mol2seq, max_sequence_length,
                smiles_fct = transformer.transform_x,
                target_fct = transformer.transform)


class Attention(pl.LightningModule):

    def __init__(self, vector_dim, sub_fragment_context_vector_dim):
        # inputs:
        # vector_dim - dimension of the input vectors sequence to the attention layer, here equal to 2 * hidden vector dimension of lstm
        #              [ (h1L , h1R) , (h2L , h2R), ..., (h60L , h60R)] for CEPDB, and then each (hiL , hiR) concatenated vector has the
        #              size as defined by the vector_dim
        # sub_fragment_context_vector_dim - this is u_sub in Zhou Li paper, must be of the same dimension (this param is then no needed)

        super().__init__()

        # equation (11) - (13) from paper [Wu20]
        # "u_sub is the sub-fragment context vector.
        # It is randomly initialized and
        # jointly learned during the network training process,
        # so are the other involved vectors, W_sub and b_sub"

        # W_sub and b_sub from eq. (11)
        self.linear = nn.Linear(vector_dim, sub_fragment_context_vector_dim)
        # tanh from eq. (11)
        self.tanh = nn.Tanh()
        # dot product from eq. (12)
        self.u_sub_linear = nn.Linear(sub_fragment_context_vector_dim, 1, bias=False)
        # softmax from eq. (12) over the dim t ('time') of the sequence
        # the sum expression of eq. (12) is not correct, it should be
        #     sum_i exp(u_i^T * u_s)
        # i.e. u_i^T instead of u_t^T
        self.softmax = nn.Softmax(dim=1)

    def forward(self, h):

        # example: h with batch size 2, max sequence length 3 and vector dim 256
        # h.size() = [2, 3, 256]

        # just for ease, we assume vector_dim = sub_fragment_context_vector_dim,
        # i.e. the matrix W_sub has quadratic shape [256, 256] and b_sub has shape [256]
        x = self.linear(h)

        # tanh is applied element-wise and doesn't change the shape
        x = self.tanh(x)
        # x.size() = [2, 3, 256]
        #
        # batch size = 2 then we process 2 molecules at the same time
        # 3 - is max_sequence_length
        # 256 - is the concatenated hidden state of bilstm
        #
        #                 1 -- 256
        # [1 - A, 1 , [ev1 (of size 256)]
        #         .
        #         .
        #         3 , [ev3] ,
        #
        # 2 - B, 1 ,  [ev1]
        #         .
        #         .
        #         3 , [ev3] ]
        #

        # next linear layer has one output neuron and no bias vector
        # and thus is equivalent to dot produxt <x, u_sub>
        # (where u_sub is the sub-fragment context vector, i.e. the trainable weights in the linear layer)
        x = self.u_sub_linear(x)
        # x.size() = [2, 3, 1]

        # softmax respects batch-processing --> alpha.size() = [2,3]
        # i.e. alpha contains two attention vectors with probability weights for t=0,1,2 as elements
        # [1 - A, 1, alpha1 (scalar value after summation)
        #         .
        #         .
        #         3, alpha3 ,
        #
        # 2 - B, 1, alpha1 ,
        #         .
        #         .
        #        3, alpha3 ]
        alpha = self.softmax(x)

        # parallel multiplication of scalar alpha_t and vector h_t for all t=1..max_sequence_length
        m = alpha * h
        # m.size() = [2, 3, 256]

        # sum along sequence index t, i.e. along the dim=1 --> [2,256]
        msum = torch.sum(m, dim=1)
        # [1 - A, [ev_summed],
        #
        # 2 - B, [ev_summed]
        #                       ]

        return msum

class BiLstmForPce(util_lightning.OscmlModule):

    def __init__(self, number_of_subgraphs, embedding_dim, lstm_hidden_dim, mlp_units, padding_index, target_mean, target_std, optimizer, mlp_dropouts=None):
        # inputs:
        # number_of_subgraphs - nr of fragments in a dictionary after processing the entire dataset sample, e.g. CEPDB (25.000 points) = 56
        # embedding_dim - dimension of the embedding vectors, each fragment type in a sequence will be represented by such vector
        # lstm_hidden_dim - dimension of the hidden lstm vectors, hardcoded to be equal to the embedding_dim
        # mlp_units - a list of mlp units numbers for each mlp hidden layer, the nr of items in that list defines the nr of hidden layers
        # padding_index - e.g. if we have 3 fragments in a sequence for a given molecule (1-2-3) and max sequence is 5, then the sequence is padded
        #                 1-2-3-P-P, P it is hardcoded for now to be always zero
        # target_mean/std - for the Z-transformation
        # optimizer - optimiser settings
        # mlp_dropouts - a list of dropouts to use in each hidden mlp layer
        #
        super().__init__(optimizer, target_mean, target_std)
        logging.info('initializing %s', locals())

        self.save_hyperparameters()

        assert len(mlp_units) > 0

        # we add +1 to number_of_subgraphs because
        # padding_idx = 0 in a sequences is mapped to zero vector
        # the extra 0-index vector is used for out of vocabulary fragment or for padded entries
        self.embedding = nn.Embedding(number_of_subgraphs+1, embedding_dim, padding_idx=padding_index)
        self.bilstm = nn.LSTM(input_size=embedding_dim, hidden_size=lstm_hidden_dim, bidirectional=True,
                        batch_first=True)
        # factor 2 because the LSTM is birectional
        lstm_output_dim = 2 * lstm_hidden_dim
        self.attention = Attention(lstm_output_dim, lstm_output_dim)

        mlp_units_with_input_dim = [lstm_output_dim]
        mlp_units_with_input_dim.extend(mlp_units)
        self.mlp = oscml.utils.util_pytorch.create_mlp(mlp_units_with_input_dim, mlp_dropouts)


    def forward(self, index_sequences):

        # get the sequences of embedding vectors corresponding to subgraph indexes
        x = self.embedding(index_sequences)

        # h is the sequence of hidden state vectors;
        # the state vectors from both LSTMs are already concatenated
        h, _ = self.bilstm(x)
        # h has shape [batch size, input sequence length, 256]
        # where 256 = lstm_output_dim = 2 * hidden state vector dim = number neurons in the first MLP layer
        #print('MY H', h.size())



        # 1. sum
        # attention mechanism, here only sum (i.e. multiplication with fixed attention weights = 1)*
        # sum_n=1..60 h_n / 60 --> one vector of size [256]
        # --> [batch size, 256]
        #attention_value_msum = torch.mean(h, dim=1)

        # 2. equation (11) - (13) from paper [Wu20]
        attention_value_msum = self.attention(h)
        # --> [batch size, 256]
        #print('MY MSUM', attention_value_msum.size(), attention_value_msum)

        o = self.mlp(attention_value_msum)
        # transform from size [batch_size, 1] to [batch_size]
        o = o.view(len(o))

        return o