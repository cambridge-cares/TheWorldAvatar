# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import uuid
import json
import os
import csv 
import cirpy 
import pybel
from rdkit import Chem
import openbabel

#print(uuid.uuid4())

def read_json(json_file):
    with open(json_file) as f:
        data = json.load(f)
    base=os.path.basename(json_file)
    name = os.path.splitext(base)[0]
    return data, name

data, name = read_json('co2_trial.json')

def chem_info(data, name):
    atoms = data['Atom types']
    coords = data['Geometry']
    geom_out = []
    for k in range(len(atoms)):
        geom_out.append(atoms[k] + ' ' + str(coords[k][0]) + ' ' + str(coords[k][1]) + ' ' + str(coords[k][2]))
    geom_string = ' '.join(geom_out)
    out_xyz = open(name + '.xyz','w')
    out_xyz.write(str(len(atoms)) + '\n')
    out_xyz.write('\n')
    for k in range(len(geom_out)):
        out_xyz.write(geom_out[k] + '\n')
    out_xyz.close()
    mol = next(pybel.readfile("xyz",name + '.xyz'))
    
    start = mol.atoms[0]
    bond_list = []
    for bond in openbabel.OBAtomBondIter( start.OBAtom):
        bond_list.append(str(bond.GetBeginAtom().GetIdx()) + ' '  +  str(bond.GetEndAtom().GetIdx()) + ' ' + str(bond.GetBO()))
    bond_string = ' '.join(bond_list)

   
    
    mol_smi = mol.write('smi')
    mol_smi = mol_smi.split('\t')[0]
    m = Chem.MolFromSmiles(mol_smi)
    inchi = Chem.MolToInchi(m)
    return geom_string, mol_smi, inchi, bond_string
    



def write_abox(data):
    out_id = uuid.uuid4()
    label = data['Empirical formula'].replace('1','') #We will take the label as the empirical formula, but without any 1s.
    alt_label = cirpy.resolve(label,'names')[0] #Take first name as a sample alternative label for now
    casid = cirpy.resolve(label,'cas')[0] #Take first CAS registry ID
    geom_string, mol_smi, inchi, bond_string = chem_info(data, name)
    
    with open('ABox_' + name + '.csv', 'w', newline='') as csvfile:
        spamwriter = csv.writer(csvfile, delimiter=',',
                                  quotechar='|', quoting=csv.QUOTE_MINIMAL)
        spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value'])
        spamwriter.writerow([out_id, 'Instance','Species','',''])
        spamwriter.writerow(['http://purl.org/dc/elements/1.1/identifier','Data Property',out_id,'',out_id])
        spamwriter.writerow(['http://www.w3.org/2000/01/rdf-schema#label','Data Property',out_id,'',label])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#altLabel','Data Property',out_id,'',alt_label])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#casRegistryID','Data Property',out_id,'',casid])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#SMILES','Data Property',out_id,'',mol_smi])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#inChI','Data Property',out_id,'',inchi])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#hasAtomicBond','Data Property',out_id,'',bond_string])
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#hasGeometry','Data Property',out_id,'',geom_string])
        
        
        