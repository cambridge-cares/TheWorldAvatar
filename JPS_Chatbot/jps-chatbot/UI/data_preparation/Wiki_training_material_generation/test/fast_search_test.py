import json
target_smiles = 'O=C(OC)C1=CC=C(C=C1)C(=CC(=O)CC(C)C)CD'
import pandas as pd


test_string = '[NH2-].[NH2-].[Ca+2'  # expect [NH2-].[NH2-].[Ca+2]

with open('../FULL_SMILES_LIST') as f:
    FULL_SMILES_LIST = json.loads(f.read())

SMILES = list(set(FULL_SMILES_LIST))

