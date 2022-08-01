# Standard library imports
import os
from pathlib import Path

# Third party imports
from configobj import ConfigObj

# Define location of properties file (with Triple Store and RDB settings)
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "properties", "config.properties"))
TSCLIENT_FILE = os.path.abspath(os.path.join(Path(__file__).parent, "properties", "tsclient.properties"))

def read_properties_file(filepath):
    """
    Reads SPARQL endpoints from properties file (as global variables)
    
    Arguments:
        filepath - absolute file path to properties file
    """
    # Define global scope for global variables to be read from properties file
    global NAMESPACE, ENDPOINT
    
    # Read properties file
    property = ConfigObj(filepath)

    # Extract namespace
    try:
        NAMESPACE = property['namespace']
    except KeyError:
        raise KeyError('Key "namespace" is missing in properties file: ' + filepath)
    if input == '':
        raise KeyError('No "namespace" value has been provided in properties file: ' + filepath)

    # Extract SPARQL endpoint of KG
    try:
        ENDPOINT = property['sparql.endpoint']+property['namespace']+"/sparql"
    except KeyError:
        raise KeyError('Key "sparql.endpoint" is missing in properties file: ' + filepath)
    if input == '':
        raise KeyError('No "sparql.endpoint" value has been provided in properties file: ' + filepath)

def read_ifc_file():
    """
    Reads IFC file located at ./data/ifc directory into required file paths
    """
    ifcpath= os.path.join('.','data', 'ifc')
    filelist = [file for file in os.listdir(ifcpath) if os.path.isfile(os.path.join(ifcpath, file))]

    global INPUT_IFC, INPUT_TTL

    if not filelist:
        raise FileNotFoundError('No ifc file is available at the ./data/ifc folder')
    elif len(filelist)==1:
        INPUT_IFC= os.path.join(ifcpath, filelist[0])
        INPUT_TTL= INPUT_IFC.split('.ifc')[0] + '.ttl'
        print(INPUT_IFC,INPUT_TTL)
    else:
        raise RuntimeError('More than one IFC file is located at the ./data/ifc folder. Please place only ONE IFC file')

def cleandir():
    """
    Remove previously generated files from the directory while keeping any input ifc models
    """
    # Get a list of all files in directory
    for rootDir, subdirs, filelist in os.walk('./data/'):
        for filename in filelist:
            try:
                filepath = os.path.join(rootDir, filename)
                if "./data/ifc" not in filepath or filepath.endswith('.ttl'):
                    os.remove(filepath)
            except OSError:
                print("Error while deleting file")
    
    # Delete any existing blazegraph databases
    if os.path.exists("blazegraph.jnl"):
        try:
            os.remove("blazegraph.jnl") 
        except OSError:
            print("Error while deleting file")

def dictfind(lst, key, value):
    """
    Find the list index containing a specific key value pair
    """    
    for i, dic in enumerate(lst):
        if dic[key] == value:
            return i
    return None

# Run when module is imported
read_properties_file(PROPERTIES_FILE)
cleandir()
read_ifc_file()