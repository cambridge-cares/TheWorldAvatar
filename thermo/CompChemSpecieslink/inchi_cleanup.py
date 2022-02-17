# -*- coding: utf-8 -*-
"""
Created on Fri Apr 16 14:40:03 2021

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

def inchi_query(): #Defines a query to get the InchI and corresponding speciesIRI as defined in the specified ontology and syntax.
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


results = query_endpoint(endpoint, inchi_query()) #Perform the query and store them in results.
speciesIRI = []
Inchi = []
#This loop puts the queried speciesIRI and Inchis in the corresponding lists.
for k in range(len(results['results']['bindings'])):
    speciesIRI.append(results['results']['bindings'][k]['speciesIRI']['value'])
    Inchi.append(results['results']['bindings'][k]['Inchi']['value'])

#cmd = "echo " + '"' + Inchi[0] + '"' + "|" + obabel_path + " -iinchi -oxyz > arg1" Command for command line openbabel
#cmd2 = "obabel_path + "-ixyz arg1 - -oinchi -" #Second part of command line openbabel 

def clean_inchi(in_inchi):
    mol = Chem.MolFromInchi(in_inchi)
    if mol: 
        out_inchi = Chem.MolToInchi(mol)
        return out_inchi
    else:
        return "Could Not clean InChI"

def clean_inchi_babel(in_inchi):
    cmd = "echo " + '"' + in_inchi + '"' + "|" + obabel_path + " -iinchi -oxyz --gen3D > arg1"
    output = subprocess.check_output(cmd, shell=True)
    cmd2 = obabel_path + " -ixyz" + " arg1" + " -oinchi"
    out2 = subprocess.check_output(cmd2, shell=True)
    out2 = out2.decode("utf-8") 
    sep = '\r' 
    out_inchi = out2.split(sep,1)[0]
    return out_inchi



cleaned_inchi = []
babel_cleaned_inchi = []

# for k in range(len(Inchi)):
#       cleaned_inchi.append(clean_inchi(Inchi[k]))

# for k in range(len(Inchi)):
#     babel_cleaned_inchi.append(clean_inchi_babel(Inchi[k]))

parser = argparse.ArgumentParser()
parser.add_argument("--inchi", "-i", help="Inchi to be converted to OpenBabel Format")
args = parser.parse_args()

if args.inchi:
    input_inchi = sys.argv[2]
    output_inchi = clean_inchi_babel(input_inchi)
    
    