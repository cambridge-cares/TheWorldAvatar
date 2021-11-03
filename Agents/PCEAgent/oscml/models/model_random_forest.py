import collections
import datetime
from time import sleep

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
import sklearn
import sklearn.svm
import sklearn.ensemble
from tqdm import tqdm

import oscml.features.fingerprint
import oscml.utils.params
import oscml.utils.util
import oscml.utils.util_sklearn

from oscml.utils.util import log
from oscml.utils.util import smiles2mol

def train_and_test_random_forest_regression_with_HPO(
    df, train_size, params_fingerprint, params_grid_RF, cross_validation):

    df['rdkitmol'] = oscml.utils.util.smiles2mol_df(df, 'smiles')
    
    search_list = []
    for pfp in params_fingerprint:
        x = oscml.features.fingerprint.get_fingerprints(df, 'rdkitmol', pfp, as_numpy_array = True)
        y = df['pceave']
        
        x_train, x_test, y_train, y_test = sklearn.model_selection.train_test_split(
            x, y, train_size=train_size, shuffle=True, random_state=0)
        
        model = sklearn.ensemble.RandomForestRegressor(criterion='mse', random_state=0, verbose=0, n_jobs=1)
        result_train, result_test, search = oscml.utils.util_sklearn.train_and_test_with_HPO(
                            x_train, y_train, x_test, y_test, 
                            model, params_grid_RF, cross_validation)
        
        t = (pfp, result_train, result_test, search)
        search_list.append(t)
    
    return search_list

def generate_balanced_bins(column, number_bin):
    log('number of bins=', number_bin)
    q = np.arange(0.0, 1.0, step=1./number_bin)
    q = np.append(q, 1.0)
    #log('q=', q)
    quantiles = column.quantile(q, interpolation='lower')
    # replace min value 0.01 by 0.0, otherwise nan will be assigned as bin number to rows 716 and 938 
    # resulting in an error when fitting RandomForestClassifier
    quantiles[0]  = 0.0
    log('quantiles=\n', quantiles)

    labels = np.arange(len(quantiles)-1)
    column_bin = pd.cut(column, bins=quantiles, labels=labels)
    print('value counts=\n', column_bin.value_counts())
    return column_bin

def train_and_test_random_forest_classifier_with_HPO(
    df, train_size, params_fingerprint, params_grid_RF, cross_validation, list_number_bin):

    df['rdkitmol'] = oscml.utils.util.smiles2mol_df(df, 'smiles')
    
    search_list = []
    
    for pfp in params_fingerprint:
        
        x = oscml.features.fingerprint.get_fingerprints(df, 'rdkitmol', pfp, as_numpy_array = True)
    
        bin_list = []
    
        for number_bin in list_number_bin:
        
            df['bin'] = generate_balanced_bins(df['pceave'], number_bin)
            y = df['bin'].to_numpy()
            
            x_train, x_test, y_train, y_test = sklearn.model_selection.train_test_split(
                x, y, train_size=train_size, shuffle=True, random_state=0)

            model = sklearn.ensemble.RandomForestClassifier(criterion='gini', random_state=0, verbose=0, n_jobs=1)
            result_train, result_test, search = oscml.utils.util_sklearn.train_and_test_with_HPO(
                                x_train, y_train, x_test, y_test, 
                                model, params_grid_RF, cross_validation, scoring='accuracy')

            t = (number_bin, result_train, result_test, search)
            bin_list.append(t)
        
        search_list.append((pfp, bin_list))
    
    return search_list
    
