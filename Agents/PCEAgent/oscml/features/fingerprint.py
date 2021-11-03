import collections
import logging
from time import sleep

import numpy as np
import pandas as pd
import rdkit
import rdkit.Avalon.pyAvalonTools
import rdkit.Chem
import rdkit.Chem.AllChem
import rdkit.Chem.rdMHFPFingerprint
import sklearn


def create_fingerprint_function(params_fingerprint):

    params = params_fingerprint.copy()
    type = params.pop('type') if 'type' in params else 'morgan'
    if type == 'morgan':
        return lambda mol : rdkit.Chem.AllChem.GetMorganFingerprintAsBitVect(mol, **params)
    else:
        raise RuntimeError('unknown fingerprint type=' + type)

def get_fingerprints(df, column, params_fingerprint, as_numpy_array = True):
    logging.info('generating fingerprints, params=' + str(params_fingerprint))
    fp_func = create_fingerprint_function(params_fingerprint)
    x = []
    for i in range(len(df)):
        m = df.iloc[i][column]
        fingerprint = fp_func(m)
        if (as_numpy_array):
            fingerprint = np.array(fingerprint, dtype=np.float32)
        x.append(fingerprint)
    return x

def normalize_fingerprints(df, column):
    st = sklearn.preprocessing.StandardScaler()
    #x = np.array(list(df[column]))
    x = list(df[column])
    x = st.fit_transform(x)
    return list(x)