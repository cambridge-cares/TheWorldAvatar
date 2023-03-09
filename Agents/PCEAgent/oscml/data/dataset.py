import collections
import logging

import numpy as np
import pandas as pd
import sklearn
import sklearn.model_selection
from tqdm import tqdm

import oscml.data.dataset_cep
import oscml.data.dataset_hopv15
import oscml.features.fingerprint
import oscml.features.weisfeilerlehman
import oscml.models.model_gnn
from oscml.utils.util import smiles2mol
import os
from oscml.kg.kgQuery import queryKG

def path_cepdb_valid_smiles(root='.'):
    return root + '/data/processed/CEPDB_valid_SMILES.csv'

def path_cepdb_25000(root='.'):
    return root + '/data/processed/CEPDB_25000.csv'

def path_hopv_15(root='.'):
    return root + '/data/raw/HOPV_15_revised_2.data'

def path_osaka(root='.'):
    return root + '/data/raw/Nagasawa_RF_SI.txt'

class DataTransformer():

    def __init__(self, column_target, target_mean, target_std, column_x=None):
        self.column_target = column_target
        self.target_mean = target_mean
        self.target_std = target_std
        self.column_x = column_x

    def transform_x(self, data):
        if self.column_x:
            return data[self.column_x]
        else:
            return data

    def transform(self, data):
        if self.column_x:
            return (data[self.column_target] - self.target_mean) / self.target_std
        else:
            return (data - self.target_mean) / self.target_std

    def inverse_transform(self, data):
        if self.column_target:
            return data[self.column_target] * self.target_std + self.target_mean
        else:
            # that means isinstance(data, torch.Tensor) because the value predicted by PyTorch
            # has to be transformed back for evaluation
            return data * self.target_std + self.target_mean

def create_transformer(df, column_target, column_x=None):
    mean = float(df[column_target].mean())
    std = float(df[column_target].std(ddof=0))
    logging.info('calculated target mean=%s, target std=%s', mean, std)
    return DataTransformer(column_target, mean, std, column_x)

def add_node2index(original, new, zero_index_for_new):

    added = original.copy()
    for node in new:
        if node not in original:
            index = (0 if zero_index_for_new else len(added))
            added[node] = index
    return added

def clean_data(df, mol2seq, column_smiles, column_target):

    mask_known = []
    for i in tqdm(range(len(df))):
        smiles = df.iloc[i][column_smiles]
        m = smiles2mol(smiles)
        contains_only_known_types = True
        if mol2seq:
            try:
                mol2seq(m)
            except:
                contains_only_known_types = False
        mask_known.append(contains_only_known_types)

    mask_known = np.array(mask_known)
    logging.info('molecules with known atom types=%s', len(df[mask_known]))
    mask_notna = df[column_target].notna().to_numpy()
    logging.info('molecules with given target value for %s = %s', column_target, len(df[mask_notna]))
    mask = np.logical_and(mask_known, mask_notna)
    df_cleaned = df[mask].copy()
    logging.info('molecules with both=%s', len(df_cleaned))

    return df_cleaned

def store(df, filepath):
    logging.info('storing %s', filepath)
    # store without the internal index of Pandas Dataframe
    df.to_csv(filepath, index=False)

def read_and_split_by_size(filepath, split_size_array, seed):
    logging.info('reading %s', filepath)
    df = pd.read_csv(filepath)

    train_size, val_size, test_size = split_size_array
    if not train_size:
        train_size = len(df) - val_size - test_size
    elif not val_size:
        val_size = len(df) - train_size - test_size
    elif not test_size:
        test_size = len(df) - train_size - val_size

    train_plus_val_size = train_size + val_size
    df_train, df_test = sklearn.model_selection.train_test_split(df,
                    train_size=train_plus_val_size, shuffle=True, random_state=seed)
    df_train, df_val = sklearn.model_selection.train_test_split(df_train,
                    train_size=train_size, shuffle=True, random_state=seed+1)
    logging.info('train=%s, val=%s, test=%s', len(df_train), len(df_val), len(df_test))

    return df_train, df_val, df_test

def read_and_split(filepath, split_column='ml_phase'):
    logging.info('reading %s', filepath)
    df = pd.read_csv(filepath)
    df_train = df[(df[split_column] == 'train')].copy()
    df_val = df[(df[split_column] == 'val')].copy()
    df_test = df[(df[split_column] == 'test')].copy()
    logging.info('split data into sets of size (train / val / test)=%s / %s / %s', len(df_train), len(df_val), len(df_test))
    return df_train, df_val, df_test

def get_dataframes(dataset, seed=200, cvFold=None, nestedCvFolds=None):

    src = dataset['src']
    x_column = dataset['x_column'][0]
    y_column = dataset['y_column'][0]
    querykg = dataset['querykg']

    if querykg:
        kg_options = dataset['kg_options']
        kg_data_file = kg_options['kgdstcsv']

        response = queryKG(kg_options['sparqlEndPoint'], "\n".join(kg_options['queryStr']))
        pces = []
        smiles = []
        for items in response:
            pces.append(items['PowerConversionEfficiencyValue'])
            smiles.append(items['DonorSMILES'])
        dd = [[x,y] for x,y in zip(smiles, pces)]
        df = pd.DataFrame(dd, columns=[x_column, y_column])
        df = df.sort_values(by=[x_column])
        df.to_csv(kg_data_file, index=False)

        df_train, df_val, df_test = read_and_split_by_size(kg_data_file, split_size_array=kg_options['split'], seed=seed)
        df_train['ml_phase'] = 'train'
        df_val['ml_phase'] = 'val'
        df_test['ml_phase'] = 'test'

        df = pd.concat([df_train,df_val,df_test])

        if nestedCvFolds:
            add_k_fold_columns(df=df, k=nestedCvFolds, seed=seed, column_name_prefix='ml_phase')

        df.to_csv(kg_data_file, index=False)

        src = kg_data_file
        dataset['split'] = 'ml_phase'

    if cvFold is not None:
        split = dataset['split'] + '_fold_'+str(cvFold)
    else:
        split = dataset['split']

    if isinstance(split, str):
        # split is the name of the split column with values train, val and test
        df_train, df_val, df_test = oscml.data.dataset.read_and_split(src, split_column=split)
        # for testing only
        #df_train, df_val, df_test = df_train[:1500], df_val[:500], df_test[:500]

    else:
        # split is an array specifying the number of samples for train, val and test set
        df_train, df_val, df_test = read_and_split_by_size(src, split_size_array=split, seed=seed)

    transformer = create_transformer(df_train, column_target=y_column, column_x=x_column)
    return (df_train, df_val, df_test, transformer)


class DatasetInfo:
    def __init__(self, id=None, mol2seq=None, node_types=None, max_sequence_length=None, max_molecule_size=0, max_smiles_length=0):
        self.id=id
        #self.column_smiles = column_smiles
        #self.column_target = column_target
        if mol2seq:
            self.mol2seq = mol2seq
        else:
            self.mol2seq = oscml.features.weisfeilerlehman.Mol2seq_WL(radius=1)
        if node_types:
            self.node_types = node_types
        else:
            self.node_types = collections.defaultdict(lambda:len(self.node_types))
        self.max_sequence_length = max_sequence_length
        self.max_molecule_size = max_molecule_size
        self.max_smiles_length = max_smiles_length

    def update(self, mol, smiles):
        self.mol2seq(mol)
        for a in mol.GetAtoms():
            node_type = (a.GetSymbol(), a.GetIsAromatic())
            self.node_types[node_type]
        self.max_molecule_size = max(self.max_molecule_size, len(mol.GetAtoms()))
        self.max_smiles_length = max(self.max_smiles_length, len(smiles))

    def number_subgraphs(self):
        return len(self.mol2seq.fragment_dict)

    def as_dict(self):
        d = {}
        d['id'] = self.id
        #d['column_smiles'] = self.column_smiles
        #d['column_target'] = self.column_target
        d['max_sequence_length'] = self.max_sequence_length
        d['max_molecule_size'] = self.max_molecule_size
        d['max_smiles_length'] = self.max_smiles_length
        d['node_types'] = dict(self.node_types)
        d['wf_r1'] = {
            'atom_dict': dict(self.mol2seq.atom_dict),
            'bond_dict': dict(self.mol2seq.bond_dict),
            'fragment_dict': dict(self.mol2seq.fragment_dict),
            'edge_dict': dict(self.mol2seq.edge_dict)
        }
        return d

def get_dataset_info(dataset):
    if dataset == oscml.data.dataset_cep.CEP25000:
        return oscml.data.dataset_cep.create_dataset_info_for_CEP25000()
    elif dataset == oscml.data.dataset_hopv15.HOPV15:
        return oscml.data.dataset_hopv15.create_dataset_info_for_HOPV15()

    raise RuntimeError('unknown dataset=' + str(dataset))

def add_k_fold_columns(df, k, seed, column_name_prefix='ml_phase'):
    kfold = sklearn.model_selection.KFold(n_splits=k, shuffle=True, random_state=seed)
    k=0
    for train_index, test_index in kfold.split(df):
        #print(len(train_index), len(test_index))
        #print(test_index[:20])
        column_name = column_name_prefix + '_fold_' + str(k)
        df[column_name] = ''
        column_index = df.columns.get_loc(column_name)
        #print('COL IND', column_index)
        df.iloc[train_index, column_index] = 'train'
        df.iloc[test_index, column_index] = 'test'
        k += 1

def add_fingerprint_columns(df, smiles_column, nBits=1048, radius=2):
    params_fg_default = {
		"type": "morgan",
		"nBits": 1048,
		"radius": 2,
		"useChirality": True,
		"useBondTypes": True
		}
    params_fg_default.update({'nBits': nBits, 'radius': radius})

    df['rdkitmol'] = oscml.utils.util.smiles2mol_df(df, smiles_column)
    fp = oscml.features.fingerprint.get_fingerprints(df, 'rdkitmol', params_fg_default, as_numpy_array = True)
    df = df.drop(columns=['rdkitmol'])

    # convert an array of arrays (i.e. an array of fingerprints) into a matrix
    fp = np.stack(fp, axis=0)
    fp = fp.astype(int)

    for bit in range(nBits):
        bit_column = fp[:,bit]
        column_name = 'fp' + str(bit)
        df[column_name] = bit_column

    return df
