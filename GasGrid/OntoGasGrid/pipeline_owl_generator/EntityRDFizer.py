##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 04 Dec 2020                      #
##########################################

"""This module is designed to convert entities of any domain and their data and metadata into RDF.
It requires the entities and their data to be provided as inputs in an ABox excel template, that is
filled in with example data and that is provided in the following path:
python/power_plnat/test/resources/ABoxOntoLandUse.csv."""

from rdflib import Graph, FOAF, URIRef, BNode, Literal
from rdflib.namespace import RDF, RDFS, Namespace
from tkinter import Tk  # from tkinter import Tk for Python 3.x
from tkinter.filedialog import askopenfilename
import csv
import uuid 
import PropertyReader as propread
import ABoxGeneration as aboxgen
import os.path as path

"""Declared column headers as constants"""
COLUMN_1 = 'Source'
COLUMN_2 = 'Type'
COLUMN_3 = 'Target'
COLUMN_4 = 'Relation'
COLUMN_5 = 'Value'
TOTAL_NO_OF_COLUMNS = 5

"""Predefined types source entries"""
TYPE_INSTANCE = 'Instance'
TYPE_DATA     = 'Data Property'

"""Utility constants"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'

"""Declared an array to maintain the list of already created instances"""
instances = dict()
g = Graph()

"""This shows a file dialog box that enables the user to select a file to convert into RDF"""
def select_file():
    """Suppresses the root window of GUI"""
    Tk().withdraw()
    """Opens a file dialog box to select a file"""
    return askopenfilename()

"""This function checks the validity of header in the ABox excel template"""
def is_header_valid(row):
    if len(row) >= TOTAL_NO_OF_COLUMNS:
        if row[0].strip().lower()==COLUMN_1.lower() \
                and row[1].strip().lower()==COLUMN_2.lower() \
                and row[2].strip().lower()==COLUMN_3.lower() \
                and row[3].strip().lower()==COLUMN_4.lower() \
                and row[4].strip().lower()==COLUMN_5.lower():
            return True
        else:
            return False

"""This function converts a row into an entity or a link between two entities or a data or annotation property value"""
def process_data(row):
    if len(row) >= TOTAL_NO_OF_COLUMNS:
        if row[0].strip() is None or row[0].strip()  == '' \
                or row[1].strip() is None or row[1].strip()  == '' \
                or row[2].strip() is None or row[2].strip()  == '':
           return

        if row[1].strip().lower() == TYPE_INSTANCE.lower():
            if (row[3].strip() is None or row[3].strip() == '') \
                    and (row[4].strip() is None or row[4].strip() == ''):

                if row[2].strip()[:4] == 'http':
                    aboxgen.create_instance(g,
                                            URIRef(row[2]),
                                            propread.getABoxIRI()+SLASH+format_iri(row[0]),
                                            row[0])
                    instances[row[0].strip()] = row[2].strip()
                else:
                    aboxgen.create_instance(g,
                                            URIRef(propread.getTBoxIRI()+HASH+format_iri(row[2])),
                                            propread.getABoxIRI()+SLASH+format_iri(row[0]),
                                            row[0])
                    instances[row[0].strip()] = row[2].strip()

            elif row[2].strip() in instances:
                if not row[0].strip() in instances or row[3].strip()  == '':
                    return
                else:
                    aboxgen.link_instance(g, URIRef(row[3]),
                                            URIRef(propread.getABoxIRI()+SLASH+format_iri(row[0].strip())),
                                            URIRef(propread.getABoxIRI()+SLASH+format_iri(row[2].strip())))

        elif row[1].strip().lower() == TYPE_DATA.lower():
            if row[2].strip() in instances and not row[4].strip() == '':
                aboxgen.link_data(g, URIRef(row[0].strip()),
                                  URIRef(propread.getABoxIRI()+SLASH+format_iri(row[2].strip())),
                                  row[4].strip())

"""Formats an IRI string to discard characters that are not allowed in an IRI"""
def format_iri(iri):
    # iri = iri.title() 
    iri = iri.replace(":"," ")
    iri = iri.replace(",", " ")
    iri = iri.replace(" ","")
    return iri

"""Converts an IRI into a namespace"""
def create_namespace(IRI):
    print(IRI)
    return Namespace(IRI)

"""This function checks the validity of the excel template header and iterates over each data row until the whole
content of the template is converted into RDF"""
def convert_into_rdf(file_path):
    file_path_name = file_path+'.csv'
    if not path.isfile(file_path_name):
        return
    with open(file_path_name, 'rt') as csvfile:
        rows = csv.reader(csvfile, skipinitialspace=True)
        line_count = 0
        for row in rows:
           if line_count == 0:
               if not is_header_valid(row):
                   break
               else:
                   global g
                   g = Graph()

           if line_count > 0:
               process_data(row)
           line_count +=1
    g.serialize(destination=file_path+propread.getABoxFileExtension(), format="application/rdf+xml")


