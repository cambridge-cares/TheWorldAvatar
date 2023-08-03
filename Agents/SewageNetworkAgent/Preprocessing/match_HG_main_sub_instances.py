################################################
# Authors: Nazanin Mashhaditafreshi (nmashhad@cmclinnovations.com) #
#          Kushagar Rustagi (krustagi@cmclinnovations.com)         #
# Date: 4 January 2023                           #
################################################

# This module finds HG labels first and then, find respective HG instance for the HG label in main and sub network

import os

import numpy as np
import pandas as pd


def create_directory(path):
    """
    Create output directory if not exists.
    """
    if not os.path.exists(path):
        os.makedirs(path)


def file_spliter(csv_file, path, file_name):
    """
    Split csv file into smaller files.

    :param csv_file: Main csv file with all columns
    :param path: Path to save output files
    :param file_name: File name
    """
    cols = csv_file.columns
    n = 4999

    cols_split = [cols[i:i + n] for i in range(0, len(cols), n)]
    for idx, sp in enumerate(cols_split):
        csv_file[sp].to_csv(os.path.join(path, f"{file_name}_{idx}.csv"), index=False, header=False)


def search_instance_main(x):
    """
    Search in HG main instance file for respective label in HG_vals file
    """
    return "|".join(hg_instances.loc['HG001'][
                        hg_instances.loc['HG001'].apply(lambda p: str(p).lower()) == str(x).lower()].index.values)


def search_instance_sub(x):
    """
    Search in HG sub instance file for respective label in HG_vals file
    """
    return "|".join(hg_instances.loc['HG011'][
                        hg_instances.loc['HG011'].apply(lambda p: str(p).lower()) == str(x).lower()].index.values)


if __name__ == '__main__':

    # Output path
    output = './Data/results/'
    create_directory(output)

    

    # File containing HG instances
    hg_instances = pd.read_csv(os.path.join(output + 'HG_instances_withoutReplacement.csv'),
                               encoding='latin1', index_col=0)

    # File name containing the connection details of sub and main connections. It is generated using the python code in pirmasens repository.
    main_sub_connection_file = './Data/raw/sub_network_connections_consolidated_wo_geodata.csv'
    df_sub_network_connection = pd.read_csv(main_sub_connection_file)

    # Grouping the connection based on the main connection and storing it in the list format.
    list_main_connection = df_sub_network_connection.groupby('HG001')['label'].apply(list).to_dict()

    # Creating pandas dataframe for the main as column header and sub connection in rows for each main connection
    hg_vals = pd.DataFrame.from_dict(list_main_connection, orient='index')
    hg_vals = hg_vals.T

    hg_vals.to_csv(os.path.join(output, 'HG_main_sub_labels.csv'), index=False)

    hg_vals = pd.read_csv(os.path.join(output, "HG_main_sub_labels.csv"), header=None)

    dict_res = {}
    # This part of code finds the name of respective HG instance for main and sub HG labels
    for idx, col in enumerate(hg_vals.columns):
        dict_res[f'{idx}'] = hg_vals[col]
        # First row is main label in hg_vals
        main_instance = hg_vals[col][0]
        dict_res[f'instances_main_{idx}'] = [search_instance_main(main_instance)] + ["" for i in
                                                                                     range(len(hg_vals[col]) - 1)]
        sub_instances = [""] + hg_vals[col][1:].apply(search_instance_sub)
        dict_res[f'instances_sub_{idx}'] = sub_instances

    df_res = pd.DataFrame(dict_res)
    df_res.to_csv(os.path.join(output + "HG_main_sub_instances.csv"), index=False)

    # This part of code processes the file HG_main_sub_instances and seperates the main and sub instances in rows and columns

    df_res = pd.read_csv(os.path.join(output + 'HG_main_sub_instances.csv'))

    # Below code processes the Main instances and Sub instances

    # Sub Instance Processing
    # Checking for the sub instances columns into different rows and expanding the column based on the '|' symbol
    df_sub = df_res.loc[:, ['_sub_' in i for i in df_res.columns]]
    list_sub_column = df_sub.columns

    for i in range(len(list_sub_column)):
        df_res[list_sub_column[i]] = df_res[list_sub_column[i]].str.split('|')
        df_res[list_sub_column[i]] = df_res[list_sub_column[i]].explode(list_sub_column[i])
        df_res[list_sub_column[i]] = df_res[list_sub_column[i]].drop_duplicates(keep='first')

    # Main Instance processing
    # Expanding the main instances column into separate columns and copying the same sub instances in below rows.
    df_main = df_res.loc[:, ['_main_' in i for i in df_res.columns]]
    df_sub = df_res.loc[:, ['_sub_' in i for i in df_res.columns]]

    df_main_sub = pd.DataFrame(np.concatenate([df_main.values, df_sub.values], axis=0), columns=df_main.columns)
    df_main_sub.columns = df_main_sub.columns.str.replace(r'_main_', '')

    combine_main_sub = df_main_sub["instances1"].str.split("|", expand=True)
    list_sub_column = df_main_sub.columns
    for i in range(len(df_main_sub.columns)):
        temp = df_main_sub[list_sub_column[i]].str.split("|", expand=True)
        for j in range(len(temp.columns)):
            temp.iloc[38:, j] = df_main_sub.loc[38:, list_sub_column[i]]
        combine_main_sub = pd.concat([combine_main_sub, temp], axis=1)
        # print(list_sub_column[i])

    combine_main_sub = pd.concat([combine_main_sub, temp], axis=1)
    combine_main_sub = combine_main_sub.dropna(axis=0, how='all')
    combine_main_sub = combine_main_sub.iloc[:, 2:]
    combine_main_sub = combine_main_sub.fillna('None')
    combine_main_sub.to_csv(os.path.join(output, 'HG_main_sub_instances_final.csv'), index=False, header=False)

    combine_main_sub = pd.read_csv(os.path.join(output, 'HG_main_sub_instances_final.csv'), header=None)
    file_spliter(combine_main_sub, output, "HG_main_sub_instances_final")


    

