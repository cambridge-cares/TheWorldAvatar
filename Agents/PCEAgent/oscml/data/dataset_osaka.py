import logging
from time import sleep

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
from tqdm import tqdm


COLUMNS_OSAKA = ['id','nickname','refno','pcemax','pceave','voc','jsc','ff',
        'mw','mn','pdi','monomer','neghomo', 'neglumo', 'bandgap', 'smiles']

ATOM_TYPES_OSAKA =  {('C', False): 0,
             ('C', True): 1,
             ('S', True): 2,
             ('H', False): 3,
             ('N', True): 4,
             ('F', False): 5,
             ('O', False): 6,
             ('Si', False): 7,
             ('O', True): 8,
             ('S', False): 9,
             ('N', False): 10,
             ('Ge', False): 11,
             ('Se', True): 12,
             ('Te', True): 13,
             ('P', False): 14,
             ('Cl', False): 15,
             ('B', False): 16} 


def read(filepath):
    logging.info('reading data from ' + filepath)
    df = pd.read_csv(filepath, sep='\t', encoding='ISO-8859-1')
    df.columns = COLUMNS_OSAKA
    logging.info('reading finished, number of molecules=' + str(len(df)))
    return df
        