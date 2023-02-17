################################################
# Authors: Nazanin Mashhaditafreshi (nmashhad@cmclinnovations.com) #
# Date: 4 January 2023                            #
################################################

# This module generates KG and HG instances from reading XML file and cleans the data for instantiation

import csv
import os
import re
import xml.etree.ElementTree as ET
import pandas as pd
import xlsxwriter


def create_directory(path):
    """
    Create output directory if not exists.
    """
    if not os.path.exists(path):
        os.makedirs(path)


def process_child(res, priority_tags, root, en_dictionary):
    """
    Read XML data and save values of priority tags in a dictionary.

    :param res: it's a dictionary to save priority tags' values from XML file
    :param priority_tags: KG-HG priority tags file
    :param root: root of XML data
    :param en_dictionary: dictionary file containing definition of available abbreviations in XML file
    """
    for child in root:
        # no more children, leaf node.
        if len(child) == 0:
            if child.tag in priority_tags:
                # Check if tag has multiple values, so save all values with "|" separator
                if res.get(child.tag, None) is not None:
                    if child.text is not None:
                        if res[child.tag] != str(child.text):
                            # Replace abbreviations with English translation
                            if (child.tag, child.text) in en_dictionary:
                                res[child.tag] = str(res[child.tag]) + "|" + str(en_dictionary[(child.tag, child.text)])
                            else:
                                res[child.tag] = str(res[child.tag]) + "|" + str(child.text)
                else:
                    if (child.tag, child.text) in en_dictionary:
                        res[child.tag] = str(en_dictionary[(child.tag, child.text)])
                    else:
                        res[child.tag] = str(child.text)
        else:
            process_child(res, priority_tags, child, en_dictionary)


def file_spliter(csv_file, path, file_name):
    """
    Split file into smaller files (split on column).

    :param csv_file: main csv file with all columns
    :param path: path to save output files
    :param file_name: file name
    """
    cols = csv_file.columns
    n = 4999

    cols_split = [cols[i:i + n] for i in range(0, len(cols), n)]
    for idx, sp in enumerate(cols_split):
        csv_file[sp].to_csv(os.path.join(path, f"{file_name}_{idx}.csv"), index=True)


if __name__ == '__main__':

    # File containing definition of the keywords in the XML data
    dictionary = './Data/raw/dictionary.csv' 
    xml_file = './Data/raw/HALTUNGEN-ALLE-5-2022.xml'
    # File containing tags included in ontology
    priority_tags_file = './Data/raw/priority_tags_in_ontology.csv'

    # Output path
    output = './Data/results/'
    create_directory(output)

    # Read dictionary for replacing abbreviations with English translation in instance CSVs
    with open(dictionary, encoding='latin1') as csvfile:
        r = csv.reader(csvfile)
        # skip header
        next(r)
        EN_dictionary = {(XML_Tag, Keyword): English_meaning for
                         XML_Tag, Keyword, German, English, HG_Instances, KG_Instances, English_meaning in r}

    # Read XML data
    tree = ET.parse(xml_file)
    root = tree.getroot()

    # Read CSV for priority tags available in ontology
    element_to_extract = pd.read_csv(priority_tags_file)

    # Dictionary to save all HG and KG instances
    dict_res = {}

    # This part of code goes through the priority tags and read and save their values from XML file
    for col in element_to_extract.columns:
        print('Processing', col)
        priority_tags = set(element_to_extract.query(f'{col}=={col}')[col].values)

        all_res = []
        for child in root:
            res = {}
            if child.tag == col:
                process_child(res, priority_tags, child, EN_dictionary)
                all_res.append(res)
        dict_res[col] = all_res

    # This part of code splits cells with multiple values to different columns
    for tag in ["HG", "KG"]:
        df = pd.DataFrame(dict_res[tag])

        for col in df.columns:
            x = df[col].str.split('|', expand=True)
            # Choose the first two values of tags with multiple values
            if 1 < len(x.columns):
                r = min(2, len(x.columns))
                df[[col + xlsxwriter.utility.xl_col_to_name(i) for i in range(0, r)]] = x.loc[:, 0:r - 1]
                df = df.drop(col, axis=1)

        res_df = df.transpose()
        res_df.columns = [tag + str(index) for index in res_df.columns]
        res_df.fillna('None', inplace=True)
        res_df.to_csv(os.path.join(output + f'{tag}_instances_withoutReplacement.csv'), encoding='latin1')

        # Replace special letters with English alphabets
        for col in res_df.columns:
            res_df[col] = res_df[col].apply(lambda x: x.translate(
                str.maketrans({'ö': 'oe', 'ü': 'ue', 'ä': 'ae', 'ß': 'ss', 'Ö': 'Oe', 'Ü': 'Ue', 'Ä': 'Ae'})))
            # Remove every letter except English alphabets and numbers
            res_df[col] = res_df[col].apply(lambda x: re.sub('[^a-zA-Z0-9.]+', '', x))

        res_df.to_csv(os.path.join(output + f'{tag}_instances.csv'), encoding='latin1')
        file_spliter(res_df, output, f'{tag}_instances')
