import collections
import logging
from time import sleep

import pandas as pd
from tqdm import tqdm

import oscml.data.dataset
import oscml.models.model_bilstm
from oscml.utils.util import smiles2mol

COLUMNS_HOPV15 = ['smiles', 'inchi',
                  # experimental data:
                  'doi', 'inchikey', 'construction', 'architecture', 'complement', 'homo', 'lumo',
                  'electrochemicalgap', 'opticalgap', 'pce', 'voc', 'jsc', 'fillfactor',
                  'prunedsmiles',
                  # conformer array
                  'numberconformers', 'conformers']

HOPV15 = 'HOPV15'

def read(filepath):
    logging.info('reading data from %s', filepath)

    molecules = []
    with open(filepath) as f:
        while True:
            molecule = [None] * len(COLUMNS_HOPV15)
            line = f.readline()
            if not line:
                break # end of file

            molecule[0] = line.strip() # smiles
            molecule[1] = f.readline().strip() # inchi
            split = f.readline().strip().split(',') # experimental data
            molecule[2:7] = split[:5]
            molecule[7:15] = map(float, split[5:])
            molecule[15] = f.readline().strip() # pruned smiles

            # iterate on conformers and read their data
            conformer_number = int(f.readline().strip())
            molecule[16] = conformer_number
            for _ in range(conformer_number):
                assert f.readline().startswith('Conformer')
                # TODO: move the following lines to another function
                for _ in range(int(f.readline().strip())):
                    # TODO: read atom data
                    line = f.readline().strip()
                for _ in range(4):
                    # TODO: read DFT functional data
                    line = f.readline().strip()

            molecules.append(molecule)

    df = pd.DataFrame(molecules, columns = COLUMNS_HOPV15)
    logging.info('reading finished, number of molecules=%s', len(df))
    return df

def clean_hopv15(df):
    columns = ['smiles', 'homo','lumo','electrochemicalgap','opticalgap', 'pce']
    mask = df['smiles'].notna()
    for c in columns:
        mask = mask & df[c].notna()
    return df[mask]

def generate_dictionaries(file_path, column_smiles, info_cep):

    df = read(file_path)

    logging.info('generating dictionaries from SMILES strings')
    # don't start from scratch
    # instead, use the dictionaries from CEP DB such that fragments that are the same
    # in CEP DB and HOPV have the same indices.
    if info_cep:
        mol2seq = oscml.models.model_bilstm.Mol2seq(radius=1, oov=False, wf=info_cep.mol2seq.wf)
        node_types = collections.defaultdict(lambda:len(node_types), info_cep.node_types)
        info = oscml.data.dataset.DatasetInfo(mol2seq=mol2seq, node_types=node_types)
    else:
        info = oscml.data.dataset.DatasetInfo()

    smiles_valid = []
    for i in tqdm(range(len(df))):
        smiles = df.iloc[i][column_smiles]
        m = smiles2mol(smiles)
        valid = bool(m)
        smiles_valid.append(valid)
        if valid:
            info.update(m, smiles)

    number_invalid_smiles = len(df) - len(smiles_valid)
    sleep(1)
    logging.info('number of invalid SMILES=%s', number_invalid_smiles)

    logging.info('number of selected DB entries=%s', len(df))
    logging.info('max length of valid SMILES=%s', info.max_smiles_length)
    logging.info('max number of atoms in molecules with valid SMILES=%s', info.max_molecule_size)

    logging.info('DatasetInfo=\n%s', info.as_dict())

    return info

def create_dataset_info_for_HOPV15():

    # the dictionary was created and logged during preprossing the entire CEPDB
    # it was copied manually here from the log file to fix the fragment-to-embedding-index mapping
    d = {'max_molecule_size': 142, 'max_smiles_length': 186, 'node_types': {('C', False): 0, ('C', True): 1, ('Se', True): 2,
        ('O', True): 3, ('N', True): 4, ('S', True): 5, ('H', False): 6, ('Si', False): 7, ('S', False): 8, ('O', False): 9,
        ('N', False): 10, ('F', False): 11}, 'wf_r1': {'atom_dict': {'C': 0, ('C', 'aromatic'): 1, ('Se', 'aromatic'): 2,
        ('O', 'aromatic'): 3, ('N', 'aromatic'): 4, ('S', 'aromatic'): 5, 'H': 6, 'Si': 7, 'S': 8, 'O': 9, 'N': 10, 'F': 11},
        'bond_dict': {'SINGLE': 0, 'DOUBLE': 1, 'AROMATIC': 2, 'TRIPLE': 3},
        'fragment_dict': {(0, ((0, 0), (0, 0), (6, 0), (6, 0))): 0, (0, ((0, 0), (0, 1), (6, 0))): 1,
        (0, ((0, 0), (0, 1), (1, 0))): 2, (1, ((0, 0), (1, 2), (4, 2))): 3, (1, ((1, 2), (1, 2), (6, 0))): 4,
        (1, ((1, 2), (1, 2), (2, 2))): 5, (2, ((1, 2), (1, 2))): 6, (1, ((1, 2), (1, 2), (3, 2))): 7, (3, ((1, 2), (1, 2))): 8,
        (1, ((1, 2), (3, 2), (6, 0))): 9, (1, ((1, 2), (1, 2), (1, 2))): 10, (1, ((1, 2), (1, 2), (4, 2))): 11, (4, ((1, 2), (5, 2))): 12,
        (5, ((4, 2), (4, 2))): 13, (1, ((1, 2), (4, 2), (6, 0))): 14, (4, ((1, 2), (1, 2))): 15, (6, ((0, 0),)): 16, (6, ((1, 0),)): 17,
        (1, ((0, 0), (1, 2), (1, 2))): 18, (1, ((0, 1), (1, 2), (1, 2))): 19, (0, ((1, 1), (6, 0), (7, 0))): 20,
        (7, ((0, 0), (0, 0), (6, 0), (6, 0))): 21, (6, ((7, 0),)): 22, (0, ((0, 0), (1, 1), (6, 0))): 23,
        (4, ((1, 2), (1, 2), (6, 0))): 24, (1, ((1, 2), (1, 2), (7, 0))): 25, (7, ((0, 0), (1, 0), (6, 0), (6, 0))): 26,
        (0, ((0, 0), (0, 1), (7, 0))): 27, (0, ((0, 1), (1, 0), (6, 0))): 28, (1, ((0, 0), (1, 2), (3, 2))): 29,
        (0, ((0, 0), (0, 0), (0, 1))): 30, (6, ((4, 0),)): 31, (0, ((0, 1), (6, 0), (7, 0))): 32, (1, ((0, 0), (1, 2), (2, 2))): 33,
        (1, ((1, 2), (2, 2), (6, 0))): 34, (1, ((1, 2), (5, 2), (6, 0))): 35, (5, ((1, 2), (1, 2))): 36, (1, ((1, 2), (1, 2), (5, 2))): 37,
        (1, ((0, 0), (1, 2), (5, 2))): 38, (0, ((0, 0), (1, 0), (6, 0), (6, 0))): 39, (0, ((0, 1), (1, 0), (7, 0))): 40,
        (1, ((4, 2), (5, 2), (6, 0))): 41, (1, ((0, 0), (4, 2), (4, 2))): 42, (1, ((4, 2), (4, 2), (6, 0))): 43,
        (1, ((0, 0), (4, 2), (5, 2))): 44, (1, ((1, 0), (1, 2), (4, 2))): 45, (1, ((1, 0), (1, 2), (3, 2))): 46,
        (1, ((1, 0), (1, 2), (5, 2))): 47, (1, ((1, 0), (1, 2), (1, 2))): 48, (1, ((1, 0), (1, 2), (2, 2))): 49,
        (1, ((1, 0), (4, 2), (4, 2))): 50, (1, ((1, 0), (4, 2), (5, 2))): 51, (0, ((4, 0), (6, 0), (6, 0), (6, 0))): 52,
        (4, ((0, 0), (1, 2), (1, 2))): 53, (0, ((1, 0), (1, 0), (6, 0), (6, 0))): 54, (7, ((1, 0), (1, 0), (6, 0), (6, 0))): 55,
        (0, ((1, 0), (6, 0), (6, 0), (6, 0))): 56, (1, ((1, 2), (5, 2), (8, 0))): 57, (8, ((0, 0), (1, 0), (9, 1), (9, 1))): 58,
        (0, ((6, 0), (6, 0), (6, 0), (8, 0))): 59, (9, ((8, 1),)): 60, (0, ((6, 0), (6, 0), (6, 0), (10, 0))): 61,
        (10, ((0, 0), (0, 0), (1, 0))): 62, (1, ((1, 2), (1, 2), (10, 0))): 63, (0, ((0, 0), (9, 1), (10, 0))): 64,
        (9, ((0, 1),)): 65, (7, ((0, 0), (0, 0), (0, 0), (0, 0))): 66, (0, ((6, 0), (6, 0), (6, 0), (7, 0))): 67,
        (1, ((1, 2), (1, 2), (11, 0))): 68, (11, ((1, 0),)): 69, (0, ((0, 3), (6, 0))): 70, (0, ((0, 3), (1, 0))): 71,
        (1, ((1, 2), (1, 2), (9, 0))): 72, (9, ((0, 0), (1, 0))): 73, (0, ((6, 0), (6, 0), (6, 0), (9, 0))): 74,
        (0, ((0, 0), (6, 0), (6, 0), (6, 0))): 75, (0, ((0, 0), (1, 0), (8, 1))): 76, (8, ((0, 1), (0, 1))): 77,
        (0, ((0, 0), (6, 0), (8, 1))): 78, (10, ((0, 0), (0, 0), (0, 0))): 79, (0, ((0, 1), (1, 0), (10, 0))): 80,
        (0, ((1, 0), (11, 0), (11, 0), (11, 0))): 81, (11, ((0, 0),)): 82, (0, ((0, 0), (0, 0), (1, 0), (1, 0))): 83,
        (10, ((1, 0), (1, 0), (1, 0))): 84, (0, ((0, 0), (10, 3))): 85, (10, ((0, 3),)): 86, (0, ((0, 0), (9, 0), (9, 1))): 87,
        (9, ((0, 0), (6, 0))): 88, (6, ((9, 0),)): 89, (0, ((0, 1), (6, 0), (6, 0))): 90, (0, ((0, 0), (0, 1), (8, 0))): 91,
        (8, ((0, 0), (0, 0))): 92, (0, ((8, 0), (8, 1), (10, 0))): 93, (8, ((0, 1),)): 94, (0, ((0, 0), (6, 0), (6, 0), (10, 0))): 95,
        (0, ((0, 0), (0, 0), (0, 0), (1, 0))): 96, (0, ((1, 0), (1, 0), (1, 0), (1, 0))): 97, (0, ((0, 0), (1, 0), (9, 1))): 98,
        (4, ((1, 0), (1, 2), (1, 2))): 99, (1, ((1, 2), (1, 2), (4, 0))): 100, (0, ((0, 1), (1, 0), (8, 0))): 101,
        (8, ((0, 0), (0, 0), (9, 1), (9, 1))): 102, (0, ((0, 3), (7, 0))): 103, (1, ((1, 2), (4, 2), (5, 2))): 104,
        (0, ((0, 0), (0, 0), (8, 1))): 105, (10, ((1, 0), (9, 0), (9, 1))): 106, (9, ((10, 1),)): 107, (9, ((10, 0),)): 108,
        (0, ((1, 0), (9, 0), (9, 1))): 109, (9, ((0, 0), (0, 0))): 110, (7, ((0, 0), (0, 0), (1, 0), (1, 0))): 111,
        (0, ((1, 0), (6, 0), (6, 0), (9, 0))): 112, (0, ((1, 0), (9, 1), (10, 0))): 113, (0, ((0, 0), (0, 0), (0, 0), (0, 0))): 114,
        (0, ((1, 0), (6, 0), (9, 1))): 115, (0, ((0, 1), (1, 0), (1, 0))): 116, (0, ((0, 0), (6, 0), (6, 0), (9, 0))): 117,
        (4, ((1, 2), (4, 2))): 118, (1, ((1, 2), (1, 2), (8, 1))): 119, (8, ((0, 1), (1, 1))): 120, (0, ((1, 0), (1, 0), (8, 1))): 121,
        (0, ((1, 0), (6, 0), (8, 1))): 122, (8, ((0, 0), (1, 0))): 123, (4, ((1, 2), (3, 2))): 124, (3, ((4, 2), (4, 2))): 125,
        (0, ((0, 0), (1, 0), (6, 0), (10, 0))): 126, (10, ((0, 0), (1, 1))): 127, (1, ((1, 2), (1, 2), (10, 1))): 128,
        (0, ((1, 0), (8, 0), (10, 1))): 129, (10, ((0, 0), (0, 1))): 130, (0, ((0, 0), (6, 0), (8, 0), (10, 0))): 131,
        (0, ((0, 1), (8, 0), (8, 0))): 132, (1, ((1, 2), (1, 2), (8, 0))): 133, (1, ((1, 2), (5, 2), (7, 0))): 134,
        (7, ((1, 0), (1, 0), (1, 0), (1, 0))): 135, (0, ((0, 0), (1, 0), (11, 0), (11, 0))): 136,
        (0, ((0, 0), (0, 0), (11, 0), (11, 0))): 137, (0, ((0, 0), (11, 0), (11, 0), (11, 0))): 138,
        (9, ((1, 0), (6, 0))): 139, (4, ((0, 0), (4, 2), (4, 2))): 140, (0, ((1, 0), (1, 0), (1, 0), (6, 0))): 141,
        (0, ((0, 0), (0, 0), (1, 0), (6, 0))): 142, (0, ((9, 0), (9, 1), (10, 0))): 143, (0, ((0, 0), (0, 0), (1, 0), (9, 0))): 144,
        (10, ((0, 0), (1, 0), (1, 0))): 145, (8, ((1, 0), (1, 0))): 146, (0, ((1, 0), (10, 3))): 147, (0, ((9, 1), (10, 0), (10, 0))): 148,
        (0, ((1, 0), (1, 0), (9, 1))): 149}, 'edge_dict': {((0, 0), 0): 0, ((0, 6), 0): 1, ((0, 0), 1): 2,
        ((0, 1), 0): 3, ((1, 1), 2): 4, ((1, 4), 2): 5, ((1, 6), 0): 6, ((1, 2), 2): 7, ((1, 3), 2): 8, ((4, 5), 2): 9,
        ((0, 1), 1): 10, ((0, 7), 0): 11, ((6, 7), 0): 12, ((4, 6), 0): 13, ((1, 7), 0): 14, ((1, 5), 2): 15, ((1, 1), 0): 16,
        ((0, 4), 0): 17, ((1, 8), 0): 18, ((0, 8), 0): 19, ((8, 9), 1): 20, ((0, 10), 0): 21, ((1, 10), 0): 22, ((0, 9), 1): 23,
        ((1, 11), 0): 24, ((0, 0), 3): 25, ((1, 9), 0): 26, ((0, 9), 0): 27, ((0, 8), 1): 28, ((0, 11), 0): 29, ((0, 10), 3): 30,
        ((6, 9), 0): 31, ((1, 4), 0): 32, ((9, 10), 1): 33, ((9, 10), 0): 34, ((4, 4), 2): 35, ((1, 8), 1): 36, ((3, 4), 2): 37,
        ((1, 10), 1): 38, ((0, 10), 1): 39}}}
    mol2seq = oscml.models.model_bilstm.Mol2seq(radius=1, oov=True, wf=d['wf_r1'])
    logging.info('number of fragment types=%s', len(mol2seq.fragment_dict))         # 150
    logging.info('number_node_types=%s', len(d['node_types']))                      # 12

    params = {
        'id': HOPV15,
        #'column_smiles': 'smiles',
        #'column_target': 'pce',
        'mol2seq': mol2seq,
        'node_types': d['node_types'],
        'max_sequence_length': 150,
        'max_molecule_size': d['max_molecule_size'],                # 142
        'max_smiles_length': d['max_smiles_length'],                # 186
    }

    info = oscml.data.dataset.DatasetInfo(**params)
    return info