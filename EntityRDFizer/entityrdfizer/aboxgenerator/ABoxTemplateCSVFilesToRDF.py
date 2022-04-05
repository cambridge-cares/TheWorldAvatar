##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 19 May 2021                      #
##########################################

"""This module can convert all ABox template CSV files available in a folder into RDF files."""

import glob,os
from entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF import convert_into_rdf

"""This function retrieves all ABox CSV template files from folder_path and calls the converter function.
The value of folder_path should be an absolute path as shown below:
folder_path = "C:/Users/.../TheWorldAvatar/JPS_Ontology/KBTemplates/ABox" """
def convert(input_folder_path,output_folder_path=None):
    """Retrieves all ABox template CSV files from the folder path given"""
    csv_files = glob.glob(os.path.join(input_folder_path, "*.csv"))
    """Iterates over the list of CSV files"""
    for csv_file in csv_files:
        convert_into_rdf(csv_file, output_folder_path)