# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import csv
from collections import Counter
from io import StringIO
from chemaboxwriters.ontocompchem.csvstagewriter import formula_clean
from compchemparser.parsers.ccgaussian_parser import FORMAL_CHARGE

onto_spec = 'http://theworldavatar.com/ontology/ontospecies/OntoSpecies.owl'
gain_pref = 'http://purl.org/gc/'
kin_pref = 'http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl'
table_pref = 'http://www.daml.org/2003/01/periodictable/PeriodicTable.owl'
unit_pref = 'http://data.nasa.gov/qudt/owl/'

def species_csv_abox_from_string(data):
    data = json.loads(data)
    gen_id = data['job_IRI']

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)

    out_id = 'Species_' + gen_id

    label = formula_clean(data['Empirical formula']) #We will take the label as the molecular formula, but without any extraneous 1s.

    spamwriter = csv.writer(csvfile, delimiter=',',
                                quotechar='|', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])

    write_prelim(spamwriter,out_id,label)
    write_identifier_geom(spamwriter,out_id,data)
    write_atom_info(spamwriter,gen_id,out_id,data)
    write_charge_info(spamwriter,gen_id,out_id,data)
    write_atoms(spamwriter,gen_id,out_id,data)
    write_molwts(spamwriter,gen_id,out_id,data)

    csvcontent = csvfile.getvalue()
    csvfile.close()
    return csvcontent

def geom_info(data):
    atoms = data['Atom types']
    coords = data['Geometry']
    geom_out = []
    for k in range(len(atoms)):
        geom_out.append(atoms[k] + ' ' + str(coords[k][0]) + ' ' + str(coords[k][1]) + ' ' + str(coords[k][2]))
    geom_string = ' '.join(geom_out)
    return geom_string

def atom_constructor(atom_list):
    pruned_atoms = list(dict.fromkeys(atom_list))
    c = Counter(atom_list)
    atom_counts = [c[x] for x in pruned_atoms]
    return pruned_atoms, atom_counts

def write_prelim(spamwriter,out_id,label):
    spamwriter.writerow([out_id, 'Instance','Species','','',''])
    spamwriter.writerow(['http://purl.org/dc/elements/1.1/identifier','Data Property',out_id,'',out_id,'String'])
    spamwriter.writerow(['http://www.w3.org/2000/01/rdf-schema#label','Data Property',out_id,'',label,'String'])

def write_identifier_geom(spamwriter,out_id,data):
    geom_string = geom_info(data)

    if data['alt_label'] is not None:
            spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#altLabel','Data Property',out_id,'',data['alt_label'],'String'])
    if data['casid'] is not None:
        spamwriter.writerow([onto_spec + '#casRegistryID','Data Property',out_id,'',data['casid'],'String'])
    spamwriter.writerow([onto_spec + '#SMILES','Data Property',out_id,'',data['smiles'],'String'])
    spamwriter.writerow([onto_spec + '#inChI','Data Property',out_id,'',data['inchi'],'String'])
    spamwriter.writerow([onto_spec + '#hasAtomicBond','Data Property',out_id,'',data['bond_string'],'String'])
    spamwriter.writerow([onto_spec + '#hasGeometry','Data Property',out_id,'',geom_string,'String'])

def write_atom_info(spamwriter,gen_id,out_id,data):
    count = 1
    cur_at = data["Atom types"][0]
    prev_at = []

    coords = ['X','Y','Z'] #The three cartesian corrdinates.
    for k in range(len(data["Atom types"])):
        if data["Atom types"][k] != cur_at: #If we encounter a new atom, we are going to reset the counter
            if prev_at:
                if data["Atom types"][k] not in prev_at:
                    count = 1 #If we have never seen this atom, we are going set the counter to 1.
                else:
                    count = prev_at.count(data["Atom types"][k]) #Otherwise, we see how many times this atom has occured previously and set the counter to that.
        #Now the atoms are written here
        spamwriter.writerow(['Atom_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count), 'Instance',gain_pref + 'Atom','','',''])
        spamwriter.writerow([out_id,'Instance','Atom_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count),gain_pref + 'hasAtom',
                                '',''])
        spamwriter.writerow([onto_spec + '#hasCanonicalPosition','Data Property','Atom_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count),
                                '',data['atoms_can_pos'][str(k)],'Integer'])
        spamwriter.writerow(['Atom_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count), 'Instance',
                                table_pref + '#' + data["Atom types"][k],gain_pref + 'isElement','',''])
        for i in range(3): #Write the atom coordinates.
            spamwriter.writerow(['AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count),
                                    'Instance',gain_pref + 'FloatValue','','',''])
            spamwriter.writerow(['Atom_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count), 'Instance',
                                'AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count)
                                ,gain_pref + 'hasAtomCoordinate' + coords[i],'',''])
            spamwriter.writerow([gain_pref + 'hasValue','Instance','AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count)
                                    ,'',data["Geometry"][k][i],'String'])
            spamwriter.writerow(['AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data["Atom types"][k] + '_' + str(count),
                                    'Instance',unit_pref + 'unit#Angstrom',gain_pref + 'hasUnit','',''])
        prev_at.append(data["Atom types"][k]) #update previous atoms
        cur_at = data["Atom types"][k] #update current atom
        count += 1

def write_charge_info(spamwriter,gen_id,out_id,data):
    charge = data[FORMAL_CHARGE]
    spamwriter.writerow(['Charge_' + gen_id,'Instance',onto_spec + '#Charge','','',''])
    spamwriter.writerow([out_id,'Instance','Charge_' + gen_id, onto_spec + '#hasCharge','',''])
    spamwriter.writerow([onto_spec + '#value','Data Property','Charge_' + gen_id,'',charge,'String'])
    spamwriter.writerow([onto_spec + '#units','Data Property','Charge_' + gen_id,'','e','String'])
    spamwriter.writerow(['MolecularFormula_'+ gen_id,'Instance',onto_spec + '#MolecularFormula','','',''])
    spamwriter.writerow([out_id,'Instance','MolecularFormula_'+gen_id,onto_spec + '#hasMolecularFormula','',''])

def write_atoms(spamwriter,gen_id,out_id,data):
    atom_list, atom_counts = atom_constructor(data['Atom types'])
    for i in range(len(atom_list)):
        spamwriter.writerow(['Element_' + atom_list[i],'Instance',kin_pref + '#Element','','',''])
        spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','Element_' + atom_list[i],kin_pref + '#hasElement','',''])
        spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance',kin_pref + '#ElementNumber','','',''])
        spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','ElementNumber_' + gen_id + '_' + str(i+1),kin_pref + '#hasElementNumber','',''])
        spamwriter.writerow([kin_pref + '#hasNumberOfElement','Data Property','ElementNumber_' + gen_id + '_' + str(i+1),'',atom_counts[i],'Integer'])
        spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance','Element_' + atom_list[i], kin_pref + '#indicatesNumberOf','',''])
    spamwriter.writerow([out_id,'Instance',onto_spec + '#Species','','',''])

def write_molwts(spamwriter,gen_id,out_id,data):
    molwt = data['molwt']
    spamwriter.writerow(['MolecularWeight_'+ gen_id,'Instance',onto_spec + '#MolecularWeight','','',''])
    spamwriter.writerow([out_id,'Instance','MolecularWeight_'+ gen_id,onto_spec + '#hasMolecularWeight','',''])
    spamwriter.writerow([onto_spec + '#value','Data Property','MolecularWeight_' + gen_id,'',molwt,'String'])
    spamwriter.writerow([onto_spec + '#units','Data Property','MolecularWeight_' + gen_id,'','g/mol','String'])

