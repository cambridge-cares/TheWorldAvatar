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
        Reads SPARQL endpoints and output directory from properties file (as global variables)
        
        Arguments:
            filepath - absolute file path to properties file
    """
    # Define global scope for global variables to be read from properties file
    global INPUT_IFC, INPUT_TTL, NAMESPACE, ENDPOINT
    
    # Read properties file
    property = ConfigObj(filepath)

    # Extract input IFC file path
    try:
        input = property['ifcfilepath']
        INPUT_IFC = input + '.ifc' 
        INPUT_TTL = input + '.ttl'
    except KeyError:
        raise KeyError('Key "ifcfilepath" is missing in properties file: ' + filepath)
    if input == '':
        raise KeyError('No "ifcfilepath" value has been provided in properties file: ' + filepath)

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

def cleandir():
    """
    Remove previously generated files from the directory while keeping any input ifc models and ttl files
    """
    # Get a list of all files in directory
    for rootDir, subdirs, filelist in os.walk('./data/'):
        for filename in filelist:
            try:
                filepath = os.path.join(rootDir, filename)
                if "./data/ifc" not in filepath:
                    os.remove(filepath)
            except OSError:
                print("Error while deleting file")
    
    # Delete any existing blazegraph databases
    if os.path.exists("blazegraph.jnl"):
        os.remove("blazegraph.jnl") 

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
