##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 04 Dec 2020                      #
##########################################
from rdflib import Graph, FOAF, URIRef, BNode, Literal
from rdflib.namespace import RDF, RDFS, Namespace
import ABoxGeneration as aboxgen
from tkinter import Tk  # from tkinter import Tk for Python 3.x
from tkinter.filedialog import askopenfilename
import csv

"""Declared column headers as constants"""
COLUMN_1 = 'Source'
COLUMN_2 = 'Type'
COLUMN_3 = 'Target'
COLUMN_4 = 'Relation'
COLUMN_5 = 'Value'

def select_file():
    """Suppresses the root window of GUI"""
    Tk().withdraw()
    """Opens a file dialog box to select a file"""
    return askopenfilename()
