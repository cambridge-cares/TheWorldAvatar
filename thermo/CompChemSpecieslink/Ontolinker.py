# -*- coding: utf-8 -*-
"""
Created on Wed Nov 25 15:24:19 2020

@author: angir
"""
import csv
import subprocess
import os
import pybel
from SPARQLWrapper import SPARQLWrapper as sparql
from SPARQLWrapper import JSON as json
from rdkit import Chem
import sys
import argparse
import re

obabel_path = '"C:\Program Files (x86)\OpenBabel-2.3.1\obabel.exe"' 


#Function that sets up a generic query from a repository (endpoint) in the KG 
def query_endpoint(endpoint, query):
    s = sparql(endpoint)
    s.setQuery(query)
    s.setReturnFormat(json)
    results = s.query().convert()
    return results

endpoint = 'http://www.theworldavatar.com/blazegraph/namespace/ontospecies/sparql' #Location of ontology to query from

def species_query(): #Defines a query to get the speciesIRI as defined in the specified ontology and syntax.
    query = """
        PREFIX species:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
        SELECT ?speciesIRI
        WHERE
        {
        ?speciesIRI rdf:type species:Species .
        } 
        """
    return query

def inchi_query(): #Defines a query to get all InchIs and corresponding speciesIRIs as defined in the specified ontology and syntax.
    query = """
        PREFIX species: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?speciesIRI ?Inchi
        WHERE
        {
        ?speciesIRI rdf:type species:Species .
        ?speciesIRI OntoSpecies:inChI ?Inchi . 
        } 
        """
    return query

def inchi_from_iri_query(IRI):
    query = """
        PREFIX species: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?speciesIRI ?Inchi
        WHERE
        {
        ?speciesIRI rdf:type species:Species .
        ?speciesIRI OntoSpecies:inChI ?Inchi . 
        FILTER REGEX(str(?speciesIRI), """ + '"' + IRI + '"' + """, "i")
        } 
        """
    return query

def smiles_from_iri_query(IRI):
    query = """
        PREFIX species: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?speciesIRI ?SMILES
        WHERE
        {
        ?speciesIRI rdf:type species:Species .
        ?speciesIRI OntoSpecies:SMILES ?SMILES .  
        FILTER REGEX(str(?speciesIRI), """ + '"' + IRI + '"' + """, "i")
        } 
        """
    return query

#This query should return a specific IRI when querying a particular InChI - courtesy of Jiaru with a mandatory escape
#contributed by me - python does strange things sometimes. Also, escaping of special characters requires substantial pain...
def spec_inchi_query(inchi_string): 
    query = """
    PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?speciesIRI ?Inchi
    WHERE
    {
    ?speciesIRI rdf:type OntoSpecies:Species .
    ?speciesIRI OntoSpecies:inChI ?Inchi .
    FILTER REGEX(str(?Inchi), REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(""" + '"' + inchi_string + '"' + """, "InChI=1S", "InChI=1"), "/t.+", ""), "/b.+", ""), "\\\\(", "\\\\\\\\("), "\\\\)", "\\\\\\\\)"), "i")
    }
    """
    #print(query)
    return query

# This is the old method which would take everything in OntoSpecies to do the linking. 
#results = query_endpoint(endpoint, inchi_query()) #Perform the query and store them in results.


# speciesIRI = []
# Inchi = []

# #This loop puts the queried speciesIRI and Inchis in the corresponding lists.
# for k in range(len(results['results']['bindings'])):
#     speciesIRI.append(results['results']['bindings'][k]['speciesIRI']['value'])
#     Inchi.append(results['results']['bindings'][k]['Inchi']['value'])


def inchipref_strip(inchi):
    #this function is to remove the inchi prefixes, as they may be slightly different (1 vs 1S for example).
    prefix1 = 'InChI=1S'
    prefix2 = 'InChI=1'
    if prefix1 in inchi:
        inchi = inchi.strip(prefix1)
    elif prefix2 in inchi:
        inchi = inchi.strip(prefix2)
    return inchi

def stereochem_strip(inchi):
    sep = '/t' #strip everything after the stereochemistry identifier -> Not all InChis are generated with stereochemisty.
    inchi = inchi.split(sep, 1)[0]
    return inchi

def optical_strip(inchi):
    sep = '/b' # Strip the enantiomer chemistry as well for now, as it seems to be causing some issues and is not necessarily standard.
    inchi = inchi.split(sep, 1)[0]
    return inchi
    
def inchi_from_log(path):
    cmd = obabel_path + " -ilog " + path + " -oinchi" #Define system call to openbabel to get Inchi from Gaussian log file.
    output = subprocess.check_output(cmd, shell=True) #Run the system call and convert it from bytes.
    output = output.decode("utf-8") 
    sep = '\r' 
    base_inchi = output.split(sep,1)[0] #Get the InchI
    test_inchi = inchipref_strip(base_inchi) #Remove the prefix for comparison purposes
    log_inchi = stereochem_strip(test_inchi) #Remove stereochemistry identifiers
    log_inchi = optical_strip(log_inchi) #Remove enantiomer identifiers
    return [log_inchi, base_inchi]

def comparator(log_inchi,Inchi,speciesIRI):
    match_flag = 0
    for k in range(len(Inchi)):
        Inchi[k] =  inchipref_strip(Inchi[k]) #Take the queried InChis from ontospecies and strip prefixes
        if Inchi[k] == log_inchi: #compare the inchis
            return [Inchi[k],speciesIRI[k]]
            match_flag = 1
    if match_flag == 0:
        return ['No Match found', 'No Match found']

#These two functions define the writing of the mapping csv file, one for writing a fresh file, and one for appending to an existing one.
def csv_writer(filename,dir_name):
    with open(dir_name + '\\' + filename + '.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=',',quotechar='|', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(['Log File Name' , 'OntoSpecies IRI']) 

def csv_appender(filename,dir_name,logfile,target):
    with open(dir_name + '\\' + filename + '.csv', 'a', newline='') as csvfile:
        writer = csv.writer(csvfile, delimiter=',',quotechar='|', quoting=csv.QUOTE_MINIMAL)
        writer.writerow([logfile , target]) 
    

first_flag = True 

def mapping(logfile,mapname,dir_name):
    path = dir_name + '\\' +  logfile
    #log_inchi = inchi_from_log(path)[0] - deprecated method
    #target = comparator(log_inchi,Inchi,speciesIRI)[1]
    log_inchi = inchi_from_log(path)[1]
    results  = query_endpoint(endpoint, spec_inchi_query(log_inchi))   
    if results['results']['bindings']:
        target = results['results']['bindings'][0]['speciesIRI']['value']
    else:
        target = 'No Match found'
    filename = os.path.basename(logfile).replace('.log','') #Get the filename without the .log
    logfilename = filename + '.log'
    global first_flag
    if first_flag == True:
        csv_writer(mapname,dir_name)
        csv_appender(mapname,dir_name,logfilename,target)
        first_flag = False
    else: 
        csv_appender(mapname,dir_name,logfilename,target)

mapname = 'mapping_file'

#if __name__=='__main__':

# Add long and short argument for command line execution of this script. It takes a directory of log files as argument.
parser = argparse.ArgumentParser()
parser.add_argument("--directory", "-d", help="Directory containing Gaussian log files to be linked")
args = parser.parse_args()

if args.directory:
    directory = sys.argv[2]
    for filename in os.listdir(directory):
        if filename.endswith(".log"):
                mapping(filename , mapname, directory)
                
        





