'''
Created on Dec 1, 2021

@author: Aleksandar Kondinski
'''

import os
base = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILE_PATHS = {
    'r1_mops_csv': os.path.join(base, 'mops_output', 'r1_mops.csv'),
    'r2_mops_csv': os.path.join(base, 'mops_output', 'r2_mops.csv'),
    'r1andr2_csv': os.path.join(base, 'mops_output', 'r1_r2.csv'),
    'kg_assembly_csv': os.path.join(base, 'mops_output', 'kg_analysis_1.csv'),
    'mops_am': os.path.join(base, 'mops_output', 'mops_am', ''),
    'r1_cbus': os.path.join(base, 'mops_output', 'r1_cbus', ''),
    'r2_cbus': os.path.join(base, 'mops_output', 'r2_cbus', ''),
    'list_R1': os.path.join(base, 'mops_output', 'list_R1.json'),
    'list_preR2': os.path.join(base, 'mops_output', 'list_preR2.json'),
    'list_R2': os.path.join(base, 'mops_output', 'list_R2.json'),
    'temp': os.path.join(base, 'mops_output', 'temp', ''),
    'mops_r1': os.path.join(base, 'mops_output', 'mops_r1', ''),
    'mops_r2': os.path.join(base, 'mops_output', 'mops_r2', '')
}
