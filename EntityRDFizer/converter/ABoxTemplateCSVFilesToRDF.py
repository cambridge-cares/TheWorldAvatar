##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 19 May 2021                      #
##########################################

"""This module can convert all ABox template CSV files available in a folder into RDF files."""

import glob,os
import converter.ABoxTemplateCSVFileToRDF as csv2rdf
from tkinter import Tk  # from tkinter import Tk for Python 3.x
from tkinter.filedialog import askdirectory

"""This shows a file dialog box that enables the user to select a file to convert into RDF"""
def select_folder():
    """Suppresses the root window of GUI"""
    Tk().withdraw()
    """Opens a file dialog box to select a file"""
    return askdirectory()

"""This function retrieves all CSV files from folder_path and calls the converter function.
The value of folder_path should be an absolute path as shown below:
folder_path = "C:/Users/.../TheWorldAvatar/JPS_Ontology/KBTemplates/ABox" """
def convert(input_folder_path, output_folder_path):
    """Retrieves all CSV files from the folder path given"""
    csv_files = glob.glob(os.path.join(input_folder_path, "*.csv"))
    """Iterates over the list of CSV files"""
    for csv_file in csv_files:
        csv2rdf.convert_into_rdf(csv_file, output_folder_path)

"""This block of codes calls the function that converts the content of all ABox CSV template files 
residing in a folder into RDF"""
if __name__ == '__main__':
    """Calls the RDF conversion function"""
    convert(select_folder(), "C:\\Users\\msff2\\Documents\\c4eWorkInProgress\\TheWorldAvatar\\JPS_Ontology\\KBTemplates\\ABoxRDFFiles\\test\\path")