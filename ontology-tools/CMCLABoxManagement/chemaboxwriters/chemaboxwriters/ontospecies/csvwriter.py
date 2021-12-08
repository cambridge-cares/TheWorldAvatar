# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import csv

from io import StringIO
from chemaboxwriters.ontocompchem.csvwriter import formula_clean
from compchemparser.parsers.ccgaussian_parser import ATOM_TYPES, \
                                                     FORMAL_CHARGE, \
                                                     EMP_FORMULA
from chemaboxwriters.ontospecies.jsonwriter import MOLWT, \
                                                   INCHI, \
                                                   SMILES, \
                                                   GEOM_STRING, \
                                                   BOND_STRING, \
                                                   PUBCHEM_ALT_LABEL, \
                                                   PUBCHEM_CID, \
                                                   CAS_NUMBER, \
                                                   ATOM_LIST, \
                                                   ATOM_COUNTS, \
                                                   SPIN_MULT
import chemaboxwriters.common.commonvars as commonv
from chemaboxwriters.common import PREFIXES


onto_spec = PREFIXES["onto_spec"]
gain_pref = PREFIXES["gain_pref"]
kin_pref = PREFIXES["kin_pref"]
table_pref = PREFIXES["table_pref"]
unit_pref = PREFIXES["unit_pref"]
spec_pref = PREFIXES["spec_pref"]


def os_csvwriter(data):
    data = json.loads(data)
    gen_id = data[commonv.ENTRY_UUID]

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                                quotechar='"', quoting=csv.QUOTE_MINIMAL)

    out_id = data[commonv.ENTRY_IRI]

    label = formula_clean(data[EMP_FORMULA]) #We will take the label as the molecular formula, but without any extraneous 1s.

    spamwriter = csv.writer(csvfile, delimiter=',',
                                quotechar='"', quoting=csv.QUOTE_MINIMAL)
    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])

    write_prelim(spamwriter,out_id,label)
    write_identifier_geom(spamwriter,out_id,data)
    write_atom_info(spamwriter,gen_id,out_id,data)
    write_charge_info(spamwriter,gen_id,out_id,data)
    write_atoms(spamwriter,gen_id,out_id,data)
    write_molwts(spamwriter,gen_id,out_id,data)

    csvcontent = csvfile.getvalue()
    csvfile.close()
    return [csvcontent]

def write_prelim(spamwriter,out_id,label):
    spamwriter.writerow(['ABoxOntoSpecies','Ontology',onto_spec,'http://www.w3.org/2002/07/owl#imports','',''])
    spamwriter.writerow(['ABoxOntoSpecies','Ontology',spec_pref[:-1],'base','',''])
    spamwriter.writerow([out_id, 'Instance','Species','','',''])
    spamwriter.writerow(['http://purl.org/dc/elements/1.1/identifier','Data Property',out_id,'',out_id,'String'])
    spamwriter.writerow(['http://www.w3.org/2000/01/rdf-schema#label','Data Property',out_id,'',label,'String'])

def write_identifier_geom(spamwriter,out_id,data):
    if PUBCHEM_ALT_LABEL in data:
        spamwriter.writerow(['http://www.w3.org/2004/02/skos/core#altLabel','Data Property',out_id,'',data[PUBCHEM_ALT_LABEL],'String'])
    if CAS_NUMBER in data:
        spamwriter.writerow([onto_spec + '#casRegistryID','Data Property',out_id,'',data[CAS_NUMBER],'String'])
    spamwriter.writerow([onto_spec + '#SMILES','Data Property',out_id,'',data[SMILES],'String'])
    spamwriter.writerow([onto_spec + '#inChI','Data Property',out_id,'',data[INCHI],'String'])
    if PUBCHEM_CID in data:
        spamwriter.writerow([onto_spec + '#pubChemCID','Data Property',out_id,'', data[PUBCHEM_CID],'String'])
    spamwriter.writerow([onto_spec + '#hasAtomicBond','Data Property',out_id,'',data[BOND_STRING],'String'])
    spamwriter.writerow([onto_spec + '#hasGeometry','Data Property',out_id,'',data[GEOM_STRING],'String'])
    spamwriter.writerow([onto_spec + '#spinMultiplicity','Data Property',out_id,'',data[SPIN_MULT],'String'])

def write_atom_info(spamwriter,gen_id,out_id,data):
    count = 1
    cur_at = data[ATOM_TYPES][0]
    prev_at = []

    coords = ['X','Y','Z'] #The three cartesian corrdinates.
    for k in range(len(data[ATOM_TYPES])):
        if data[ATOM_TYPES][k] != cur_at: #If we encounter a new atom, we are going to reset the counter
            if prev_at:
                if data[ATOM_TYPES][k] not in prev_at:
                    count = 1 #If we have never seen this atom, we are going set the counter to 1.
                else:
                    count = prev_at.count(data[ATOM_TYPES][k]) #Otherwise, we see how many times this atom has occured previously and set the counter to that.
        #Now the atoms are written here
        spamwriter.writerow(['Atom_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count), 'Instance',gain_pref + 'Atom','','',''])
        spamwriter.writerow([out_id,'Instance','Atom_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count),gain_pref + 'hasAtom',
                                '',''])
        spamwriter.writerow(['Atom_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count), 'Instance',
                                table_pref + '#' + data[ATOM_TYPES][k],gain_pref + 'isElement','',''])
        for i in range(3): #Write the atom coordinates.
            spamwriter.writerow(['AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count),
                                    'Instance',gain_pref + 'FloatValue','','',''])
            spamwriter.writerow(['Atom_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count), 'Instance',
                                'AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count)
                                ,gain_pref + 'hasAtomCoordinate' + coords[i],'',''])
            spamwriter.writerow([gain_pref + 'hasValue','Instance','AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count)
                                    ,'',data["Geometry"][k][i],'String'])
            spamwriter.writerow(['AtomCoordinate' + coords[i] + '_' + gen_id + '_' + data[ATOM_TYPES][k] + '_' + str(count),
                                    'Instance',unit_pref + 'unit#Angstrom',gain_pref + 'hasUnit','',''])
        prev_at.append(data[ATOM_TYPES][k]) #update previous atoms
        cur_at = data[ATOM_TYPES][k] #update current atom
        count += 1

def write_charge_info(spamwriter,gen_id,out_id,data):
    if FORMAL_CHARGE in data:
        charge = data[FORMAL_CHARGE]

        spamwriter.writerow(['Charge_' + gen_id,'Instance',onto_spec + '#Charge','','',''])
        spamwriter.writerow([out_id,'Instance','Charge_' + gen_id, onto_spec + '#hasCharge','',''])
        spamwriter.writerow([onto_spec+'#value','Data Property','Charge_' + gen_id,'',charge,'String'])
        spamwriter.writerow([onto_spec+'#units','Data Property','Charge_' + gen_id,'','e','String'])
        spamwriter.writerow(['MolecularFormula_'+ gen_id,'Instance',onto_spec + '#MolecularFormula','','',''])
        spamwriter.writerow([out_id,'Instance','MolecularFormula_'+gen_id,onto_spec + '#hasMolecularFormula','',''])
        
def write_atoms(spamwriter,gen_id,out_id,data):
    atom_list = data[ATOM_LIST]
    atom_counts = data[ATOM_COUNTS]
    for i in range(len(atom_list)):
        spamwriter.writerow(['Element_' + atom_list[i],'Instance',kin_pref + '#Element','','',''])
        spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','Element_' + atom_list[i],kin_pref + '#hasElement','',''])
        spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance',kin_pref + '#ElementNumber','','',''])
        spamwriter.writerow(['MolecularFormula_' + gen_id,'Instance','ElementNumber_' + gen_id + '_' + str(i+1),kin_pref + '#hasElementNumber','',''])
        spamwriter.writerow([kin_pref + '#hasNumberOfElement','Data Property','ElementNumber_' + gen_id + '_' + str(i+1),'',atom_counts[i],'Integer'])
        spamwriter.writerow(['ElementNumber_' + gen_id + '_' + str(i+1),'Instance','Element_' + atom_list[i], kin_pref + '#indicatesNumberOf','',''])
    spamwriter.writerow([out_id,'Instance',onto_spec + '#Species','','',''])

def write_molwts(spamwriter,gen_id,out_id,data):
    if MOLWT in data:
        molwt = data[MOLWT]
        spamwriter.writerow(['MolecularWeight_'+ gen_id,'Instance',onto_spec + '#MolecularWeight','','',''])
        spamwriter.writerow([out_id,'Instance','MolecularWeight_'+ gen_id,onto_spec + '#hasMolecularWeight','',''])
        spamwriter.writerow([onto_spec + '#value','Data Property','MolecularWeight_' + gen_id,'',molwt,'String'])
        spamwriter.writerow([onto_spec + '#units','Data Property','MolecularWeight_' + gen_id,'','g/mol','String'])