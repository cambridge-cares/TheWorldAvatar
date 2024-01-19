# -*- coding: utf-8 -*-
"""
Created on Wed Mar 10 14:41:10 2021

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
import tempfile
import Ontolinker

#The purpose of this script is to close the loop and produce the updated 'hasgeometry' tag for OntoSpecies from an OntoCompChem
#Entry (a Gaussian Log File)

obabel_path = '"C:\Program Files (x86)\OpenBabel-2.3.1\obabel.exe"' 
folder_path = 'C:\\Users\\angir\\OneDrive\\DOCUME~1-DESKTOP-HF92O2R-21527423'

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

def xyz_from_log(path):
    cmd = obabel_path + " -ilog " + path + " -oxyz" #Define system call to openbabel to get XYZ from Gaussian log file.
    output = subprocess.check_output(cmd, shell=True) #Run the system call and convert it from bytes.
    output = output.decode("utf-8") 
    file = open('output.xyz','w')
    file.write(output)
    file.close()
    file_cleaner('output.xyz')
    return output

def read_xyz(filename):
   """Read filename in XYZ format and return lists of atoms and coordinates.

   If number of coordinates do not agree with the statd number in
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

def geom_string_gen(atoms,coords):
    geom_out = []
    for k in range(len(atoms)):
        geom_out.append(atoms[k] + ' ' + str(coords[k][0]) + ' ' + str(coords[k][1]) + ' ' + str(coords[k][2]))
    geom_string = ' '.join(geom_out)
    return geom_string

xyz_from_log('co2_freq_g09.log') 
atoms, coordinates, mult = read_xyz('output.xyz')
geom_string = geom_string_gen(atoms, coordinates) 

IRI = 'http://www.theworldavatar.com/kb/ontospecies/s00009360.owl/Species_7258652483811000'


with open('ABox_Geom.csv', 'w', newline='') as csvfile:
    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value'])
    spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasGeometry',
                                 'Data Property', IRI, '' , geom_string])


  
