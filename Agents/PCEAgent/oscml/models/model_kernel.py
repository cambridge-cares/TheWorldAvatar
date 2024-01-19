import collections
import datetime
from time import sleep

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import rdkit
import rdkit.Chem
import rdkit.Chem.AllChem
import sklearn
import sklearn.svm
import sklearn.ensemble

import oscml.features.fingerprint
import oscml.utils.params
import oscml.utils.util

from oscml.utils.util import log

def get_matrix_fingerprint_tanimoto(X, Y=None):
    if Y is None:
        Y = X

    size_X = len(X)
    size_Y = len(Y)
    #log('get_matrix_fingerprint_tanimoto', type(X), size_X, type(Y), size_Y)

    m = np.zeros((size_X, size_Y), dtype=float)
    for i in range(size_X):
        for j in range(size_Y):
            # calculates by defait tanimoto similarity
            m[i,j] = rdkit.DataStructs.FingerprintSimilarity(X[i], Y[j])

    return m

def rbf_kernel_phys_and_struct(X, Y=None, gamma_structural=0., gamma_physical=0., alpha=None):
    """
    The code was copied from https://scikit-learn.org/stable/modules/generated/sklearn.metrics.pairwise.rbf_kernel.html#sklearn.metrics.pairwise.rbf_kernel .
    The Euclidean distance between vectors was replaced by a distance between fingerprints.

    During training this method is called with X = X_train and Y = transposed X_train, and
    the method returns a kernel matrix of shape (len(X_train), len(X_train)).
    During test this method is called with X = X_test and Y = transposed X_train, and
    the method returns a kernel matrix of shape (len(X_test), len(X_train)).
    See also https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html#sklearn.svm.SVR.predict
    If the kernel method does not implement this in a correct way, a ValueError maybe raised,
    e.g X.shape[1] = 303 should be equal to 900, the number of samples at training time
    """
    #y_size = None if Y is None else len(Y)
    #log('kernel was called with len(X)=', len(X), ', len(Y)=', y_size)

    #if gamma_structural is None:
    #    gamma = 1.0 / X.shape[1]
    #    gamma_structural = 1.0 #??? TODO-AE

    # distance for between two fingerprints = 1 - Tanimoto similarity
    # i.e. their distance is between 0 and 1

    # log('gamma_structural=', gamma_structural, 'gamma_physical', gamma_physical, 'alpha=', alpha)

    if gamma_physical == 0.:
        X_tmp = X[:,0]
        Y_tmp = Y[:,0]
        K_struct = (-1 * gamma_structural) * (1 - get_matrix_fingerprint_tanimoto(X_tmp, Y_tmp))
        np.exp(K_struct, K_struct)
        return K_struct
    elif gamma_structural == 0.:
        K_phys = (-1 * gamma_physical) * sklearn.metrics.pairwise.euclidean_distances(X[:,1:], Y[:,1:], squared=True)
        np.exp(K_phys, K_phys)
        return K_phys

    K_struct = (-1 * gamma_structural) * (1 - get_matrix_fingerprint_tanimoto(X[:,0], Y[:,0]))
    K_phys = (-1 * gamma_physical) * sklearn.metrics.pairwise.euclidean_distances(X[:,1:], Y[:,1:], squared=True)

    if alpha:
        # first exp, then add weighted kernels
        np.exp(K_struct, K_struct) # exponentiate K_struct in-place
        np.exp(K_phys, K_phys)
        K = alpha * K_struct + (1 - alpha) * K_phys
    else:
        # first add, then exp
        K = (K_struct + K_phys)
        np.exp(K, K)  # exponentiate K in-place
    return K

def preprocess_data_phys_and_struct(df, params_fingerprint, train_size, column_smiles,
                                    columns_phys, column_y, scaler_svr_physical_data = None):
    df_copy = df.copy()

    if column_smiles:
        df_copy['rdkitmol'] = oscml.utils.util.smiles2mol_df(df_copy, column_smiles)
        df_copy['fingerprint'] = oscml.features.fingerprint.get_fingerprints(df_copy, 'rdkitmol', params_fingerprint, as_numpy_array = False)
        x_struct = df_copy['fingerprint'].to_numpy()
        x_struct = x_struct.reshape(len(x_struct),1)
        #log('structural data:', type(x_struct), x_struct.shape)
        x = x_struct

    if columns_phys:
        x_phys = df_copy[columns_phys]
        if scaler_svr_physical_data:
            x_phys = scaler_svr_physical_data.transform(x_phys)
        else:
            scaler_svr_physical_data = sklearn.preprocessing.StandardScaler()
            x_phys = scaler_svr_physical_data.fit_transform(x_phys)

        log('mean of physical data=', scaler_svr_physical_data.mean_)
        log('variance of physical data=', scaler_svr_physical_data.var_)
        log('physical data:', type(x_phys), x_phys.shape)
        x = x_phys

    if column_smiles and columns_phys:
        x = np.hstack((x_struct, x_phys))
        log('combined data x:', type(x), x.shape)

    y = df_copy[column_y].to_numpy()
    #log('y:', type(y), y.shape)

    #x_train, x_test, y_train, y_test = sklearn.model_selection.train_test_split(x, y, train_size=train_size, shuffle=True, random_state=0)
    #log('number of samples, train=', len(x_train), ', test=', len(x_test))

    return x, y, scaler_svr_physical_data #x_train, x_test, y_train, y_test, scaler_svr_physical_data

class SVRWrapper(sklearn.svm.SVR):

    def __init__(self,
                 # params for class SVR
                 kernel='rbf', degree=3, gamma='scale', coef0=0.0,
                 tol=1e-3, C=1.0, epsilon=0.1,
                 shrinking=True, cache_size=200, verbose=False, max_iter=-1,
                 # additional kernel params
                 gamma_structural=0.0, gamma_physical=0.0, alpha=None):

        self.kernel = kernel
        self.gamma_structural = gamma_structural
        self.gamma_physical = gamma_physical
        self.alpha = alpha
        self.gamma = gamma
        self.kernel_params = {'kernel': kernel, 'gamma_structural': gamma_structural,
                'gamma_physical': gamma_physical, 'alpha': alpha, 'gamma': gamma}

        svr_params = {'kernel':kernel, 'degree':degree, 'gamma':gamma, 'coef0':coef0, 'tol':tol,
                    'C':C, 'epsilon':epsilon, 'shrinking':shrinking, 'cache_size':cache_size,
                    'verbose':verbose, 'max_iter':max_iter}

        kwargs = self.update_kernel_func(svr_params, self.kernel_params)
        super().__init__(**kwargs)

    def update_kernel_func(self, svr_params, kernel_params):
        kernel = svr_params['kernel']

        if kernel in ['rbf', 'linear', 'poly', 'sigmoid']:
            return svr_params
        else:
            if kernel == 'rbf_kernel_phys_and_struct':
                kernel_func = lambda X, y : rbf_kernel_phys_and_struct(X, y,
                                    gamma_structural = kernel_params['gamma_structural'],
                                    gamma_physical = kernel_params['gamma_physical'],
                                    alpha = kernel_params['alpha'])
            else:
                raise RuntimeError('unknown kernel')

            updated_svr_params = svr_params.copy()
            updated_svr_params.update({'kernel':kernel_func})
            return updated_svr_params

    def fit(self, X, y, sample_weight=None):
        #log('fitting SVR with params=', self.get_params())
        return super().fit(X, y, sample_weight)

    def get_params(self, deep=False):
        params = super().get_params(deep).copy()
        params.update(self.kernel_params)
        return params

    def set_params(self, **params):

        p = params.copy()
        if 'kernel' in p:
            v = p['kernel']
            self.kernel = v
        if 'gamma_structural' in p:
            v = p.pop('gamma_structural')
            self.gamma_structural = v
        if 'gamma_physical' in p:
            v = p.pop('gamma_physical')
            self.gamma_physical = v
        if 'alpha' in p:
            v = p.pop('alpha')
            self.alpha = v
        if 'gamma' in p:
            v = p['gamma']
            self.gamma = v

        kernel_params = {}
        kernel_params['kernel'] = self.kernel
        kernel_params['gamma_structural'] = self.gamma_structural
        kernel_params['gamma_physical'] = self.gamma_physical
        kernel_params['alpha'] = self.alpha
        kernel_params['gamma'] = self.gamma
        self.kernel_params = kernel_params

        params = self.update_kernel_func(p, self.kernel_params)
        return super().set_params(**params)