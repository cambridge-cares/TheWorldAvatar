# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import uuid
import json
import os
import re
import csv 
import cirpy 
import pybel
from rdkit import Chem
from rdkit.Chem import Descriptors
import openbabel
import pubchempy as pcp
from collections import Counter

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
    molwt = Descriptors.MolWt(m)
    charge = Chem.GetFormalCharge(m)
    inchi = Chem.MolToInchi(m)
    return geom_string, mol_smi, inchi, bond_string, molwt, charge
    
def get_substructure_cas(smiles):
    cas_rns = []
    results = pcp.get_synonyms(smiles, 'smiles', searchtype='substructure')
    for result in results:
        for syn in result.get('Synonym', []):
            match = re.match('(\d{2,7}-\d\d-\d)', syn)
            if match:
                cas_rns.append(match.group(1))
    return cas_rns

def atom_constructor(atom_list):
    pruned_atoms = list(dict.fromkeys(atom_list))
    c = Counter(atom_list)
    atom_counts = [c[x] for x in pruned_atoms]
    return pruned_atoms, atom_counts

def write_abox(json_file):
    data, name = read_json(json_file)
    gen_id = str(uuid.uuid4())
    out_id = 'Species_' + gen_id
    label = data['Empirical formula'].replace('1','') #We will take the label as the molecular formula, but without any 1s.
    geom_string, mol_smi, inchi, bond_string, molwt, charge = chem_info(data, name)
    atom_list, atom_counts = atom_constructor(data['Atom types'])
    try:
        c = pcp.get_compounds(inchi, 'inchi')[0]
        #alt_label = cirpy.resolve(label,'names')[0] #Take first name as a sample alternative label for now
        #casid = cirpy.resolve(label,'cas')[0] #Take first CAS registry ID
        alt_label = c.synonyms[0]
        casid = get_substructure_cas(mol_smi)[0]
    except:
        alt_label = None
        casid = None    
    with open('ABox_' + name + '.csv', 'w', newline='') as csvfile:
        spamwriter = csv.writer(csvfile, delimiter=',',
                                  quotechar='|', quoting=csv.QUOTE_MINIMAL)
        spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])
        spamwriter.writerow([out_id, 'Instance','Species','','',''])
        spamwriter.writerow(['http://purl.org/dc/elements/1.1/identifier','Data Property',out_id,'',out_id,'String'])
        spamwriter.writerow(['http://www.w3.org/2000/01/rdf-schema#label','Data Property',out_id,'',label,'String'])
        if alt_label is not None:
            spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#altLabel','Data Property',out_id,'',alt_label,'String'])
        if casid is not None:
            spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#casRegistryID','Data Property',out_id,'',casid,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#SMILES','Data Property',out_id,'',mol_smi,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#inChI','Data Property',out_id,'',inchi,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasAtomicBond','Data Property',out_id,'',bond_string,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasGeometry','Data Property',out_id,'',geom_string,'String'])
        spamwriter.writerow(['Charge_' + gen_id,'Instance','http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Charge','','',''])
        spamwriter.writerow([out_id,'Instance','Charge_' + gen_id,'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasCharge','',''])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value','Data Property','Charge_' + gen_id,'',charge,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#units','Data Property','Charge_' + gen_id,'','e','String'])
       # spamwriter.writerow([out_id,'Instance','http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species','','',''])
        spamwriter.writerow(['MolecularFormula_'+ gen_id,'Instance','http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#MolecularFormula','','',''])
        spamwriter.writerow([out_id,'Instance','MolecularFormula_'+gen_id,'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasMolecularFormula','',''])
        for i in range(len(atom_list)):
            spamwriter.writerow(['Element_' + atom_list[i],'Instance','http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#Element','','',''])
            spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','Element_' + atom_list[i],'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasElement','',''])
            spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance','http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#ElementNumber','','',''])
            spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','ElementNumber_' + gen_id + '_' + str(i+1),'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasElementNumber','',''])
            spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#hasNumberOfElement','Data Property','ElementNumber_' + gen_id + '_' + str(i+1),'',atom_counts[i],'Integer'])
            spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance','Element_' + atom_list[i],'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#indicatesNumberOf','',''])
        spamwriter.writerow([out_id,'Instance','http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species','','',''])
        spamwriter.writerow(['MolecularWeight_'+ gen_id,'Instance','http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#MolecularWeight','','',''])
        spamwriter.writerow([out_id,'Instance','MolecularWeight_'+ gen_id,'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasMolecularWeight','',''])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value','Data Property','MolecularWeight_' + gen_id,'',molwt,'String'])
        spamwriter.writerow(['http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#units','Data Property','MolecularWeight_' + gen_id,'','g/mol','String'])
        