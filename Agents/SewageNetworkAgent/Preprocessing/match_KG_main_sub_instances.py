################################################
# Authors: Nazanin Mashhaditafreshi (nmashhad@cmclinnovations.com) #
# Date: 4 January 2023                           #
################################################

# This module finds respective KG instance for the KG label in main and sub network

import os
import re
import pandas as pd
import numpy as np


def create_directory(path):
    """
    Create output directory if not exists.
    """
    if not os.path.exists(path):
        os.makedirs(path)


def search_KG001_labels(KG_file_name):
    """
    Read ttl file to extract KG labels under KG001 tag in main and sub network.

    :param KG_file_name: Name of file containing KG labels
    """
    KG_file = open(KG_file_name, "r")
    res = []
    for x in KG_file:
        m = re.search('.*KG001 (.+) ;', x)
        if m:
            found = m.group(1)
            res.append(found)
    return res


def search_instance(x):
    """
    Search for KG instance of respective KG label.
    """
    x = x.replace('"', '')
    return "|".join(
        kg_instances.loc['KG001'][kg_instances.loc['KG001'].apply(lambda p: p.lower()) == x.lower()].index.values)


if __name__ == '__main__':
    # Files containing KG labels for main and sub network
    main_ttl = './Data/raw/main_datamodel.ttl'
    sub_ttl = './Data/raw/sub_datamodel.ttl'

    # Output path
    output = './Data/results/'
    create_directory(output)

    # File containing KG instances
    kg_instances = pd.read_csv(os.path.join(output, 'KG_instances_withoutReplacement.csv'),
                               encoding='latin1', index_col=0)

    # Dictionary to save KG labels of main and sub network
    dict_res = {'main_dataModel': search_KG001_labels(main_ttl), 'sub_dataModel': search_KG001_labels(sub_ttl)}

    kg001_vals = pd.DataFrame(dict_res)
    kg001_vals.to_csv(os.path.join(output + 'KG001_values.csv'), index=False)

    # Find name of KG instances for respective main and sub KG labels
    kg001_vals['main_instances'] = kg001_vals['main_dataModel'].apply(search_instance)
    kg001_vals['sub_instances'] = kg001_vals['sub_dataModel'].apply(search_instance)

    # Save main and sub instances of KG in separate csvs
    for net in ['main', 'sub']:
        kg001_vals_df = pd.DataFrame()
        kg001_vals_df[f'{net}_instances'] = kg001_vals[f'{net}_instances'].apply(lambda x: x.strip()).replace('', np.nan).dropna()
        kg001_vals_df.to_csv(os.path.join(output, f'KG{net}Network.csv'), index=False)


