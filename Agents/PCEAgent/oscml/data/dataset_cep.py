import logging
import math
from time import sleep

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from tqdm import tqdm

import oscml.data.dataset
import oscml.features.weisfeilerlehman
from oscml.utils.util import smiles2mol

CEP25000 = 'CEP25000'



def skip_invalid_smiles(df, smiles_column_name):

    logging.info('checking SMILES')
    info = oscml.data.dataset.DatasetInfo()

    smiles_valid = []
    for i in tqdm(range(len(df))):
        smiles = df.iloc[i][smiles_column_name]
        m = smiles2mol(smiles)
        valid = bool(m)
        smiles_valid.append(valid)
        if valid:
            info.update(m, smiles)

    df['SMILES_valid'] = smiles_valid
    number_invalid_smiles = (df['SMILES_valid'] == False).sum()
    sleep(1)
    logging.info('number of invalid SMILES=%s', number_invalid_smiles)
    mask = df['SMILES_valid']
    df = df[mask]
    df = df.copy()
    df = df.drop(columns=['SMILES_valid'])
    logging.info('number of selected DB entries=%s', len(df))
    logging.info('max length of valid SMILES=%s', info.max_smiles_length)
    logging.info('max number of atoms in molecules with valid SMILES=%s', info.max_molecule_size)

    return df, info

def store_CEP_with_valid_SMILES(path_source, path_dest, numbersamples=-1):
    logging.info('reading %s', path_source)
    df_source = pd.read_csv(path_source)
    if numbersamples > 0:
        df_source = df_source[:numbersamples]
    df_dest, info = skip_invalid_smiles(df_source, 'SMILES_str')
    logging.info('returned DatasetInfo=\n%s', info.as_dict())
    oscml.data.dataset.store(df_dest, path_dest)

def skip_all_small_pce_values(df, threshold):
    mask = (df['pce'] >= threshold)
    size_total = len(df)
    df = df[mask]
    size_larger = len(df)
    logging.info('number of pce values smaller threshold=%s', size_total - size_larger)
    logging.info('number of selected DB entries=%s', size_larger)
    return df.copy()

def sample_down_small_pce_values(df, threshold, percentage):
    """
    skip each sample with PCE value < DOWN_THRESHOLD
    with probability DOWN_SKIP_PERCENTAGE
    """
    mask = (df['pce'] < threshold)
    st = len(df[mask])
    logging.info('ST = number of pce values smaller threshold=%s', st)
    logging.info('expected number to skip = ST * percentage=%s', st * percentage)
    binomial = np.random.binomial(1, percentage, len(df))
    df['skip'] = binomial
    mask = (df['pce'] >= threshold) | (1 - df['skip'])
    df = df[mask]
    df = df.drop(columns=['skip'])
    logging.info('number of selected DB entries=%s', len(df))
    return df.copy()

def clean_data(df, skip_invalid_smiles_flag = False, min_row = None, max_row = None,
               threshold_skip = None, threshold_downsampling = None, threshold_percentage = None):
    df_cleaned = df[min_row:max_row].copy()
    max_smiles_length = None
    max_smiles_atoms = None
    if skip_invalid_smiles_flag:
        df_cleaned, max_smiles_length, max_smiles_atoms = skip_invalid_smiles(df_cleaned, 'SMILES_str')
    if threshold_skip:
        df_cleaned = skip_all_small_pce_values(df_cleaned, threshold_skip)
    if threshold_downsampling and threshold_percentage:
        df_cleaned = sample_down_small_pce_values(df_cleaned, threshold_downsampling, threshold_percentage)
    return df_cleaned, max_smiles_length, max_smiles_atoms

def sample_without_replacement(df, number_samples, step=0.2, add_bin=False):
    if not isinstance(number_samples, int):
        df_array=[]
        df_rest = df
        for n in number_samples:
            df_sampled, df_rest = sample_without_replacement(df_rest.copy(), n, step)
            df_array.append(df_sampled)
        return df_array

    max_value = df['pce'].max()
    logging.info('max PCE value=%s', max_value)
    # add 0.0005 to avoid problems with the including the endpoint in np.arange below
    factor = math.ceil(max_value / step) + 0.0005
    upper = factor * step
    bins = np.arange(0.0, upper, step)
    number_bins = len(bins)-1
    logging.info('sampling without replacement, number of bins=%s', number_bins)
    logging.info('bins=%s', bins)
    labels = np.arange(number_bins)
    column_bin = pd.cut(df['pce'], bins=bins, labels=labels)

    if add_bin:
        # only for visualization of the distribution with a histogram
        df['bin'] = column_bin
    df_sampled, df_rest = train_test_split(df, train_size=number_samples, shuffle=True,
                                                random_state=0, stratify=column_bin)
    logging.info('number of selected DB entries=%s', len(df_sampled))
    logging.info('remaining DB entries for sampling=%s', len(df_rest))
    return df_sampled, df_rest

def read(filepath, threshold, number_samples, add_bin=False):
    logging.info('reading data from %s', filepath)
    df_cleaned = pd.read_csv(filepath)
    df_cleaned = skip_all_small_pce_values(df_cleaned, threshold)
    df_cleaned = sample_without_replacement(df_cleaned, number_samples, add_bin)
    logging.info('reading finished, number of molecules=%s', len(df_cleaned))
    return df_cleaned

def store_CEP_cleaned_and_stratified(src, dst, number_samples, threshold_skip, threshold_downsampling = None, threshold_percentage = None):
    logging.info('reading data from %s', src)
    df = pd.read_csv(src)
    df = skip_all_small_pce_values(df, threshold_skip)
    if threshold_downsampling:
        df = sample_down_small_pce_values(df, threshold_downsampling, threshold_percentage)
    df_train, df_val, df_test = sample_without_replacement(df, number_samples)
    df_train['ml_phase'] = 'train'
    df_val['ml_phase'] = 'val'
    df_test['ml_phase'] = 'test'
    df = pd.concat([df_train, df_val, df_test])
    logging.info('reading finished, number of molecules=%s', len(df))
    if dst:
        oscml.data.dataset.store(df, dst)
    return df

def store_CEP_cleaned_and_random(src, dst, number_samples, threshold_skip, threshold_downsampling = None, threshold_percentage = None, random_state=0):
    logging.info('reading data from %s', src)
    df = pd.read_csv(src)
    df = skip_all_small_pce_values(df, threshold_skip)
    if threshold_downsampling:
        df = sample_down_small_pce_values(df, threshold_downsampling, threshold_percentage)
    size_train_val = number_samples[0] + number_samples[1]
    size_all = size_train_val + number_samples[2]
    df_train_plus_val_plus_test, _ = train_test_split(df, train_size = size_all, shuffle=True, random_state=random_state)
    df_train_plus_val, df_test = train_test_split(df_train_plus_val_plus_test, train_size=size_train_val, shuffle=True, random_state=random_state)
    df_train, df_val = train_test_split(df_train_plus_val, train_size=number_samples[0], shuffle=True, random_state=random_state)
    df_train = df_train.copy()
    df_val = df_val.copy()
    df_test = df_test.copy()
    df_train['ml_phase'] = 'train'
    df_val['ml_phase'] = 'val'
    df_test['ml_phase'] = 'test'
    df = pd.concat([df_train, df_val, df_test])
    logging.info('reading finished, number of molecules=%s', len(df))
    if dst:
        oscml.data.dataset.store(df, dst)
    return df

def create_dataset_info_for_CEP25000():

    # the dictionary was created and logged during preprossing the entire CEPDB
    # it was copied manually here from the log file to fix the fragment-to-embedding-index mapping
    d = {'max_molecule_size': 53, 'max_smiles_length': 83, 'node_types': {('C', False): 0, ('C', True): 1, ('Se', True): 2, ('O', True): 3, ('N', True): 4, ('S', True): 5, ('H', False): 6, ('Si', False): 7}, 'wf_r1': {'atom_dict': {'C': 0, ('C', 'aromatic'): 1, ('Se', 'aromatic'): 2, ('O', 'aromatic'): 3, ('N', 'aromatic'): 4, ('S', 'aromatic'): 5, 'H': 6, 'Si': 7}, 'bond_dict': {'SINGLE': 0, 'DOUBLE': 1, 'AROMATIC': 2}, 'fragment_dict': {(0, ((0, 0), (0, 0), (6, 0), (6, 0))): 0, (0, ((0, 0), (0, 1), (6, 0))): 1, (0, ((0, 0), (0, 1), (1, 0))): 2, (1, ((0, 0), (1, 2), (4, 2))): 3, (1, ((1, 2), (1, 2), (6, 0))): 4, (1, ((1, 2), (1, 2), (2, 2))): 5, (2, ((1, 2), (1, 2))): 6, (1, ((1, 2), (1, 2), (3, 2))): 7, (3, ((1, 2), (1, 2))): 8, (1, ((1, 2), (3, 2), (6, 0))): 9, (1, ((1, 2), (1, 2), (1, 2))): 10, (1, ((1, 2), (1, 2), (4, 2))): 11, (4, ((1, 2), (5, 2))): 12, (5, ((4, 2), (4, 2))): 13, (1, ((1, 2), (4, 2), (6, 0))): 14, (4, ((1, 2), (1, 2))): 15, (6, ((0, 0),)): 16, (6, ((1, 0),)): 17, (1, ((0, 0), (1, 2), (1, 2))): 18, (1, ((0, 1), (1, 2), (1, 2))): 19, (0, ((1, 1), (6, 0), (7, 0))): 20, (7, ((0, 0), (0, 0), (6, 0), (6, 0))): 21, (6, ((7, 0),)): 22, (0, ((0, 0), (1, 1), (6, 0))): 23, (4, ((1, 2), (1, 2), (6, 0))): 24, (1, ((1, 2), (1, 2), (7, 0))): 25, (7, ((0, 0), (1, 0), (6, 0), (6, 0))): 26, (0, ((0, 0), (0, 1), (7, 0))): 27, (0, ((0, 1), (1, 0), (6, 0))): 28, (1, ((0, 0), (1, 2), (3, 2))): 29, (0, ((0, 0), (0, 0), (0, 1))): 30, (6, ((4, 0),)): 31, (0, ((0, 1), (6, 0), (7, 0))): 32, (1, ((0, 0), (1, 2), (2, 2))): 33, (1, ((1, 2), (2, 2), (6, 0))): 34, (1, ((1, 2), (5, 2), (6, 0))): 35, (5, ((1, 2), (1, 2))): 36, (1, ((1, 2), (1, 2), (5, 2))): 37, (1, ((0, 0), (1, 2), (5, 2))): 38, (0, ((0, 0), (1, 0), (6, 0), (6, 0))): 39, (0, ((0, 1), (1, 0), (7, 0))): 40, (1, ((4, 2), (5, 2), (6, 0))): 41, (1, ((0, 0), (4, 2), (4, 2))): 42, (1, ((4, 2), (4, 2), (6, 0))): 43, (1, ((0, 0), (4, 2), (5, 2))): 44, (1, ((1, 0), (1, 2), (4, 2))): 45, (1, ((1, 0), (1, 2), (3, 2))): 46, (1, ((1, 0), (1, 2), (5, 2))): 47, (1, ((1, 0), (1, 2), (1, 2))): 48, (1, ((1, 0), (1, 2), (2, 2))): 49, (1, ((1, 0), (4, 2), (4, 2))): 50, (1, ((1, 0), (4, 2), (5, 2))): 51, (0, ((4, 0), (6, 0), (6, 0), (6, 0))): 52, (4, ((0, 0), (1, 2), (1, 2))): 53, (0, ((1, 0), (1, 0), (6, 0), (6, 0))): 54, (7, ((1, 0), (1, 0), (6, 0), (6, 0))): 55}, 'edge_dict': {((0, 0), 0): 0, ((0, 6), 0): 1, ((0, 0), 1): 2, ((0, 1), 0): 3, ((1, 1), 2): 4, ((1, 4), 2): 5, ((1, 6), 0): 6, ((1, 2), 2): 7, ((1, 3), 2): 8, ((4, 5), 2): 9, ((0, 1), 1): 10, ((0, 7), 0): 11, ((6, 7), 0): 12, ((4, 6), 0): 13, ((1, 7), 0): 14, ((1, 5), 2): 15, ((1, 1), 0): 16, ((0, 4), 0): 17}}}
    mol2seq = oscml.models.model_bilstm.Mol2seq(radius=1, oov=True, wf=d['wf_r1'])
    logging.info('number of fragment types=%s', len(mol2seq.fragment_dict))         # 56
    logging.info('number_node_types=%s', len(d['node_types']))                      # 8

    params = {
        'id': CEP25000,
        #'column_smiles': 'SMILES_str',
        #'column_target': 'pce',
        'mol2seq': mol2seq,
        'node_types': d['node_types'],
        'max_sequence_length': 60,
        'max_molecule_size': d['max_molecule_size'],                # 53
        'max_smiles_length': d['max_smiles_length'],                # 83
    }

    info = oscml.data.dataset.DatasetInfo(**params)
    return info