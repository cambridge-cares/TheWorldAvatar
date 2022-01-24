# -*- coding: utf-8 -*-
"""
Created on Wed Mar  3 14:02:49 2021

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

def smiles_query(): #Defines a query to get the SMILES and corresponding speciesIRI as defined in the specified ontology and syntax.
    query = """
        PREFIX species: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX OntoSpecies: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?speciesIRI ?SMILES
        WHERE
        {
        ?speciesIRI rdf:type species:Species .
        ?speciesIRI OntoSpecies:SMILES ?SMILES . 
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

# The following commented code is the old approach that takes everything from OntoSpecies and then manipulates the necessary strings in python.
# It has been replaced by direct and specific sparql queries that achieve the same task but with substantial speedup. 
# The old method is preserved below as a backup/for legacy purposes, but should not be necessary. 


#results = query_endpoint(endpoint, inchi_query()) #Perform the query and store them in results.
#results_2 = query_endpoint(endpoint, smiles_query()) #Perform the query and store them in results.

# speciesIRI = []
# Inchi = []

# speciesIRI2 = []
# read_smiles = []

# #This loop puts the queried speciesIRI and Inchis in the corresponding lists.
# for k in range(len(results['results']['bindings'])):
#     speciesIRI.append(results['results']['bindings'][k]['speciesIRI']['value'])
#     Inchi.append(results['results']['bindings'][k]['Inchi']['value'])

# #This loop does the same thing, but for the SMILES query.
# for k in range(len(results_2['results']['bindings'])):
#     speciesIRI2.append(results_2['results']['bindings'][k]['speciesIRI']['value'])
#     read_smiles.append(results_2['results']['bindings'][k]['SMILES']['value'])



# stand_inchis = [] #We are going to edit the inchi prefixes to the standard one. 

# for x in Inchi:
#     if 'InChI=1' in x and 'InChI=1S' not in x:
#         y = x.replace('InChI=1', 'InChI=1S')
#         stand_inchis.append(y)
#     else:
#         stand_inchis.append(x)

# names = [os.path.splitext(x)[0] for x in speciesIRI]
# names2 = [os.path.splitext(x)[0] for x in speciesIRI2]
    
# names = [x.split('/')[-2] for x in names] #Store the names from the speciesIRI
# names2 = [x.split('/')[-2] for x in names2] #Store the names from the speciesIRI2

def file_cleaner(file):
    with open(file,'r') as f:
        lines = f.readlines()
        head_lines = lines[0:2]
        body_lines = lines[3:]
    #print(head_lines)
    #print(body_lines)
    with open(file,'w') as f:
        for line in head_lines:
            f.write(line)
        for line in body_lines:
            if not line.isspace():
                f.write(line)

def inchi_to_xyz(folder_path,IRI):
    #Deprecated old approach.
    # for k in range(len(speciesIRI)):
    #     if speciesIRI[k] == IRI:
    #         name = names[k].split('.')[0]
    #         inchi = stand_inchis[k]
    
    results  = query_endpoint(endpoint, inchi_from_iri_query(IRI))
    name = (results['results']['bindings'][0]['speciesIRI']['value'].split('/')[-2]).split('.')[0]
    inchi = results['results']['bindings'][0]['Inchi']['value']
    if 'InChI=1' in inchi and 'InChI=1S' not in inchi:
         inchi = inchi.replace('InChI=1', 'InChI=1S')
    else:
         inchi = inchi
    
    fp = open(folder_path + '\\' + name + '.inchi', 'w')
    fp.write(inchi)
    inchi_file = fp.name
    #print(inchi_file)
    cmd = obabel_path + " -iinchi " + inchi_file + " -oxyz --gen3D" 
    fp.close()
    output = subprocess.check_output(cmd, shell=True) 
    output = output.decode("utf-8")
    outfile = open(folder_path + '\\'+ name + '.xyz','w')
    outfile.write(output)
    outfile.close()
    file_cleaner(folder_path + '\\'+ name + '.xyz')
    return name

def smi_to_xyz(folder_path,IRI):
    #Deprecated old approach 
    # for k in range(len(speciesIRI2)):
    #         if speciesIRI2[k] == IRI:
    #             name = names2[k].split('.')[0]
    #             smiles = read_smiles[k]
    
    results  = query_endpoint(endpoint, smiles_from_iri_query(IRI))
    name = (results['results']['bindings'][0]['speciesIRI']['value'].split('/')[-2]).split('.')[0]
    smiles = results['results']['bindings'][0]['SMILES']['value']
    mol = pybel.readstring('smi', smiles)
    mol.make3D()
    output = pybel.Outputfile("xyz", folder_path + '\\' + name + '.xyz',overwrite=True)
    output.write(mol)
    output.close()
    return name

def read_xyz(filename):
   """Read filename in XYZ format and return lists of atoms and coordinates.

   If number of coordinates do not agree with the stated number in
   the file it will raise a ValueError.
   """

   atoms = []
   coordinates = []

   xyz = open(filename)
   #n_atoms = int(xyz.readline())
   #title = xyz.readline()
   lines = xyz.readlines()[2:]
   for line in lines:
       atom,x,y,z = line.split()
       atoms.append(atom)
       coordinates.append(["{:.5f}".format(float(x)), "{:.5f}".format(float(y)), "{:.5f}".format(float(z))])
   xyz.close()
   
   n_el = 0
   for i in range(len(atoms)):
       if atoms[i] == 'C':
           n_el += 6
       elif atoms[i] == 'H':
           n_el += 1
       elif atoms[i] == 'O':
           n_el += 8 
       elif atoms[i] == 'N':
           n_el += 7
       elif atoms[i] == 'S':
           n_el += 16
       elif atoms[i] == 'Si':
           n_el += 14
       elif atoms[i] == 'F':
           n_el += 9
       elif atoms[i] == 'Se':
           n_el += 34
   if n_el%2 == 0:
       mult = 1
   else:
       mult = 2
   
   return atoms, coordinates, mult


def write_gauss(filename, atoms, coordinates,mult,job_type,functional,bset):
    gauss_out = open(filename, "w")
    gauss_out.write("%NProcShared=20\n")
    gauss_out.write("%mem=32GB\n")
    gauss_out.write("%Chk="+os.path.basename(filename).replace(".com",'') +".chk" + "\n")
    gauss_out.write("#n" + " " + functional +"/" + bset + " " + job_type + "\n")
    gauss_out.write("\n")
    gauss_out.write(os.path.basename(filename).replace(".com",'')+"\n")
    gauss_out.write("\n")
    gauss_out.write("0 " + str(mult) + "\n")
    for i in range(len(atoms)):
        gauss_out.write(atoms[i] + "          " + coordinates[i][0] + "        " + coordinates[i][1] + "        " + coordinates[i][2] + "\n")
    gauss_out.write("\n")
    gauss_out.close()

def scan_edit(filename,functional,bset): #This function will edit a gaussian log file in order to run a potential energy surface scan
    gauss_file = open(filename, "r")
    gauss_lines = gauss_file.readlines()
    gauss_lines[3] = ("#n" + " " + functional +"/" + bset + " " + "Opt=ModRedundant" + "\n") #set upt the modredundant input
    gauss_file = open(filename, "w")
    gauss_file.writelines(gauss_lines)
    gauss_file.write("B " + input("First atom in Bond ") + " " + input("Second atom in Bond ") + " " + "S " + 
                     input("Number of scan steps ") + " " + input("step size "))
    gauss_file.write("\n")
    gauss_file.close()

parser = argparse.ArgumentParser()
parser.add_argument("--directory", "-d", help="Directory to write output files to", required = True)
parser.add_argument("--owl", "-o", help="Species IRI or owl file to perform calculation for", required = True)
parser.add_argument("--inchi", "-i", help="Write Gaussian file based on inchi string")
parser.add_argument("--smiles", "-s", help="Write Gaussian file based on SMILES string")
parser.add_argument("--pes", "-p", help="Add potential energy surface scan of bond to input file")
args = parser.parse_args()

if args.inchi:
    folder_path = sys.argv[2]
    IRI = sys.argv[4]
    name = inchi_to_xyz(folder_path, IRI)
    atoms, coordinates, mult = read_xyz(folder_path + '\\' +  name + '.xyz')                
    write_gauss(folder_path + '\\'  + name + '.com', atoms, coordinates, mult, "Opt Freq", "B3LYP", "6-31G(d)")

if args.smiles:
    folder_path = sys.argv[2]
    IRI = sys.argv[4]
    name = smi_to_xyz(folder_path, IRI)
    atoms, coordinates, mult = read_xyz(folder_path + '\\' + name + '.xyz')                
    write_gauss(folder_path + '\\'  + name + '.com', atoms, coordinates, mult, "Opt Freq", "B3LYP", "6-31G(d)")

if args.pes:
    scan_edit(folder_path + '\\'  + name + '.com',"B3LYP","6-31G(d)")

if args.pes and (args.inchi is None and args.smiles is None):
    parser.error("--pes requires --inchi or --smiles.")

