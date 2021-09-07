# -*- coding: utf-8 -*-
"""
Created on Fri Jul 16 12:22:29 2021

@author: angir
"""

import json
import csv
from io import StringIO
import re
import chemaboxwriters.common.commonvars as commonv
from  chemaboxwriters.ontocompchem.prefixes import comp_pref, \
                                                   data_pref, \
                                                   onto_pref, \
                                                   onto_spec, \
                                                   has_spec, \
                                                   gain_pref, \
                                                   table_pref, \
                                                   unit_pref

formula_clean_re = re.compile('(?<=[a-zA-Z])(1)(?=[a-zA-Z]+?|$)')

end_suf = '.owl' #We are writing owl files

def oc_csvwriter(data):
    data = json.loads(data)
    spec_IRI=data[commonv.SPECIES_IRI]
    calc_id = data[commonv.ENTRY_UUID]
    entryIRI = data[commonv.ENTRY_IRI]

    csvfile = StringIO(newline='')

    spamwriter = csv.writer(csvfile, delimiter=',',
                            quotechar='"', quoting=csv.QUOTE_MINIMAL)

    spamwriter.writerow(['Source', 'Type', 'Target', 'Relation','Value','Data Type'])

    write_initial(spamwriter,entryIRI,calc_id,spec_IRI)
    write_mols(spamwriter,calc_id, data)
    write_level_of_theory(spamwriter,calc_id,data)
    write_name(spamwriter,calc_id,data)
    write_frequencies(spamwriter,entryIRI,calc_id,data)
    write_rotations(spamwriter,entryIRI,calc_id,data)
    write_geom_type(spamwriter,entryIRI,calc_id,data)
    write_zpe(spamwriter,entryIRI,calc_id,data)
    write_scf(spamwriter,entryIRI,calc_id,data)
    write_occ(spamwriter,entryIRI,calc_id,data)
    write_virt(spamwriter,entryIRI,calc_id,data)
    write_geom_opt(spamwriter,entryIRI,calc_id,data)
    write_atom_info(spamwriter,calc_id,data)
    write_metadata(spamwriter,calc_id,data)

    csvcontent = csvfile.getvalue()
    csvfile.close()
    return csvcontent

def dict_to_list(d):
    dictlist = []
    for key, value in d.items():
        temp = [key,value]
        dictlist.append(temp)
    return dictlist

def formula_clean(formula):
    #A function to clean up formulae (as by default, they are written with '1's, which are removed here i.e C1O2 -> CO2)
    # [a-zA-Z]+?(1)([a-zA-Z]+?|$) ??
    clean_form = formula_clean_re.sub('',formula)
    return clean_form

def write_initial(spamwriter,jobIRI, calc_id,spec_IRI):
    #This is all the initialization part of the ABox
    spamwriter.writerow(['ABoxOntoCompChem','Ontology',onto_pref,'http://www.w3.org/2002/07/owl#imports','',''])
    spamwriter.writerow(['ABoxOntoCompChem','Ontology',comp_pref[:-1],'base','',''])
    spamwriter.writerow([jobIRI, 'Instance',onto_pref + '#G09','','',''])
    if spec_IRI: #If you have the ontospecies IRI, it puts it here. Otherwise, it leaves it out.
        spamwriter.writerow([spec_IRI, 'Instance', onto_spec,'','',''])
        spamwriter.writerow([jobIRI,'Instance',spec_IRI,has_spec,'',''])
    spamwriter.writerow([comp_pref + 'InitializationModule_' + calc_id
                         ,'Instance', onto_pref + '#InitializationModule','','','']) #Sets up initialization.
    spamwriter.writerow([jobIRI,'Instance',
                         comp_pref + 'InitializationModule_' + calc_id ,
                         onto_pref + '#hasInitialization','',''])
    spamwriter.writerow([comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                     ,'Instance', gain_pref + 'SourcePackage','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                    comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule',
                     onto_pref + '#hasEnvironment','',''])
    spamwriter.writerow([comp_pref + 'MoleculeProperty_' + calc_id
                     ,'Instance', gain_pref + 'MoleculeProperty','','',''])
    spamwriter.writerow([ comp_pref + 'InitializationModule_' + calc_id
                         ,'Instance', comp_pref + 'MoleculeProperty_' + calc_id
                         ,gain_pref + 'hasMoleculeProperty','',''])

def write_mols(spamwriter,calc_id,data):
    #This section starts the representation of the molecule, namely dividing the species into sub-molecules that contain the different atom types.
    #This will hopefully be changed by an update in OntoCompChem later.
    at_count = dict_to_list(data["Atom counts"])
    for k in range(len(at_count)): #For each atom in the molecule, make a molecule object (This is the way it's done atm.)
        spamwriter.writerow([comp_pref + 'Molecule_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,'Instance', gain_pref + 'Molecule','','',''])
        spamwriter.writerow([comp_pref + 'MoleculeProperty_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,'Instance',comp_pref + 'Molecule_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1])),
                              gain_pref + 'hasMolecule','',''])
        spamwriter.writerow([comp_pref + 'Atom_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,'Instance',gain_pref + 'Atom','','',''])
        spamwriter.writerow([comp_pref + 'Molecule_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,'Instance', comp_pref + 'Atom_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,gain_pref + 'hasAtom','',''])
        spamwriter.writerow([table_pref + '#' + at_count[k][0],'Instance',table_pref + '#Element','','',''])
        spamwriter.writerow([comp_pref + 'Atom_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                              ,'Instance',table_pref + '#' + at_count[k][0],gain_pref + 'isElement','',''])
        spamwriter.writerow([gain_pref + 'hasNumberOfAtoms','Data Property',
                             comp_pref + 'Atom_' + calc_id  + '_' + at_count[k][0] + str(float(at_count[k][1]))
                             ,'',at_count[k][1],''])

def write_level_of_theory(spamwriter,calc_id,data):
    #This section writes the information related to the level of theory for the ABox (method and basis set).
    spamwriter.writerow([comp_pref + 'LevelOfTheory_' + calc_id,
                         'Instance',onto_pref + "#LevelOfTheory",'','',''])
    spamwriter.writerow([comp_pref + 'MethodologyFeature_' + calc_id + '_LevelofTheoryParameter',
                     'Instance',gain_pref + "MethodologyFeature",'','',''])
    spamwriter.writerow([comp_pref + 'InitializationModule_' + calc_id
                         ,'Instance', comp_pref + 'MethodologyFeature_' + calc_id + '_LevelofTheoryParameter'
                         ,gain_pref + 'hasParameter','',''])
    spamwriter.writerow([onto_pref + '#hasLevelOfTheory','Data Property'
                         ,comp_pref + 'MethodologyFeature_' + calc_id + '_LevelofTheoryParameter'
                         , '',data["Method"],''])
    spamwriter.writerow([comp_pref + 'BasisSet_' + calc_id,
                 'Instance',gain_pref + "BasisSet",'','',''])
    spamwriter.writerow([comp_pref + 'InitializationModule_' + calc_id
                         ,'Instance',comp_pref + 'BasisSet_' + calc_id
                         ,gain_pref + 'hasParameter','',''])
    spamwriter.writerow([gain_pref + 'hasBasisSet','Data Property'
                     ,comp_pref + 'BasisSet_' + calc_id
                     , '','"{}"'.format(data["Basis set"]),'']) #Note that the string formatting is used to escape the ',' in basis sets.

def write_name(spamwriter,calc_id,data):
    #This writes the name of the species, taken as the formula, but with extraneous 1s removed.
    spamwriter.writerow([gain_pref + 'hasName','Data Property'
                     ,comp_pref + 'MoleculeProperty_' + calc_id
                     , '',formula_clean(data['Empirical formula']),''])

def write_frequencies(spamwriter,jobIRI, calc_id,data):
    #This section writes the vibrations to the ABox (if they exist).
    if 'Frequencies' in data:
        spamwriter.writerow([comp_pref + 'VibrationalAnalysis_' + calc_id
                         ,'Instance',gain_pref + 'VibrationalAnalysis','','',''])
        spamwriter.writerow([jobIRI,'Instance',
                            comp_pref + 'VibrationalAnalysis_' + calc_id,
                             gain_pref +'isCalculationOn','',''])
        spamwriter.writerow([comp_pref + 'Frequency_' + calc_id
                         ,'Instance',gain_pref + 'Frequency','','',''])
        spamwriter.writerow([comp_pref + 'VibrationalAnalysis_' + calc_id
                         ,'Instance',comp_pref + 'Frequency_' + calc_id
                         ,gain_pref+'hasResult','',''])
        spamwriter.writerow([onto_pref + '#hasFrequencies','Data Property'
                             ,comp_pref + 'Frequency_' + calc_id ,
                             ''," ".join(str(i) for i in data["Frequencies"]),''])
        spamwriter.writerow([gain_pref + 'hasVibrationCount','Data Property'
                             ,comp_pref + 'Frequency_' + calc_id ,
                             '',data["Frequencies number"],''])
        spamwriter.writerow([gain_pref + 'cm-1','Instance',unit_pref + 'qudt#FrequencyUnit','','',''])
        spamwriter.writerow([comp_pref + 'Frequency_' + calc_id ,
                             'Instance',gain_pref + 'cm-1',gain_pref + 'hasUnit','',''])

def write_rotations(spamwriter,jobIRI, calc_id,data):
        #This section writes the rotational constants information - rotational symmetry, rotational constants, and their values/units.
        spamwriter.writerow([comp_pref + 'RotationalSymmetry_'+ calc_id
                     ,'Instance',onto_pref + '#RotationalSymmetry','','',''])
        spamwriter.writerow([jobIRI,'Instance',
                             comp_pref + 'RotationalSymmetry_'+ calc_id ,
                             gain_pref +'isCalculationOn','',''])
        if "Rotational symmetry number" in data:
            spamwriter.writerow([onto_pref + '#hasRotationalSymmetryNumber','Data Property',
                                 comp_pref + 'RotationalSymmetry_'+ calc_id ,
                                 '',int(data["Rotational symmetry number"]),''])
        spamwriter.writerow([comp_pref + 'RotationalConstants_'+ calc_id
                     ,'Instance',onto_pref + '#RotationalConstants','','',''])

        spamwriter.writerow([jobIRI,'Instance',
                            comp_pref + 'RotationalConstants_'+ calc_id ,
                             gain_pref +'isCalculationOn','',''])
        spamwriter.writerow([onto_pref + '#hasRotationalConstants','Data Property',
                             comp_pref + 'RotationalConstants_'+ calc_id ,
                             ''," ".join(str(i) for i in data["Rotational constants"]),''])
        spamwriter.writerow([onto_pref + '#hasRotationalConstantsCount','Data Property',
                     comp_pref + 'RotationalConstants_'+ calc_id ,
                     '',data["Rotational constants number"],''])
        spamwriter.writerow([comp_pref + 'RotationalConstants_'+ calc_id ,
                             'Instance',unit_pref + 'unit#GigaHertz',gain_pref + 'hasUnit','',''])

def write_geom_type(spamwriter,jobIRI,calc_id,data):
    #This section writes the geometry type information.
    spamwriter.writerow([comp_pref + 'GeometryType_' + calc_id
                    ,'Instance',onto_pref + '#GeometryType','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                            comp_pref + 'GeometryType_' + calc_id,
                            gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([onto_pref + '#hasGeometryType','Data Property',
                            comp_pref + 'GeometryType_' + calc_id,
                            '',data["Geometry type"],''])

def write_zpe(spamwriter,jobIRI,calc_id,data):
    #This section writes the zero-point energy information (if it exists). Note that this requires a frequency calculation to be computed.
    if "Electronic and ZPE energy" in data and 'Electronic energy' in data:
        spamwriter.writerow([comp_pref + 'ZeroPointEnergy_' + calc_id
                 ,'Instance',onto_pref + '#ZeroPointEnergy','','',''])
        spamwriter.writerow([jobIRI,'Instance',
                     comp_pref + 'ZeroPointEnergy_' + calc_id,
                     gain_pref +'isCalculationOn','',''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ZeroPointEnergy'
                             ,'Instance',gain_pref + 'FloatValue','','',''])
        spamwriter.writerow([comp_pref + 'ZeroPointEnergy_' + calc_id
                 ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_ZeroPointEnergy'
                 , gain_pref + 'hasElectronicEnergy','',''])
        spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                            comp_pref + 'FloatValue_' + calc_id + '_ZeroPointEnergy',
                             '',data["Electronic and ZPE energy"] - data["Electronic energy"],''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ZeroPointEnergy','Instance',
                             unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])
    elif 'Electronic and ZPE energy' in data:
        spamwriter.writerow([comp_pref + 'ElectronicAndZPEEnergy_' + calc_id
                        ,'Instance',onto_pref + '#ElectronicAndZPEEnergy','','',''])
        spamwriter.writerow([jobIRI,'Instance',
                comp_pref + 'ElectronicAndZPEEnergy_' + calc_id,
                    gain_pref +'isCalculationOn','',''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ElectronicAndZPEEnergy'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
        spamwriter.writerow([comp_pref + 'ElectronicAndZPEEnergy_' + calc_id
                ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_ElectronicAndZPEEnergy'
            , gain_pref + 'hasElectronicEnergy','',''])
        spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                            comp_pref + 'FloatValue_' + calc_id + '_ElectronicAndZPEEnergy',
                            '', data["Electronic and ZPE energy"],''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ElectronicAndZPEEnergy'
                                ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])

def write_scf(spamwriter,jobIRI,calc_id,data):
    if 'Electronic energy' in data:
        #This section writes the electronic (SCF) energy information.
        spamwriter.writerow([comp_pref + 'ScfEnergy_' + calc_id
                        ,'Instance',onto_pref + '#ScfEnergy','','',''])
        spamwriter.writerow([jobIRI,'Instance',
                comp_pref + 'ScfEnergy_' + calc_id,
                    gain_pref +'isCalculationOn','',''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ScfEnergy'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
        spamwriter.writerow([comp_pref + 'ScfEnergy_' + calc_id
                ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_ScfEnergy'
            , gain_pref + 'hasElectronicEnergy','',''])
        spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                            comp_pref + 'FloatValue_' + calc_id + '_ScfEnergy',
                            '', data["Electronic energy"],''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_ScfEnergy'
                                ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])

def write_occ(spamwriter,jobIRI,calc_id,data):
    #This section writes the information on the occupied orbitals: HOMO, HOMO-1, HOMO-2 energies.
    #HOMO
    spamwriter.writerow([comp_pref + 'HomoEnergy_' + calc_id
                     ,'Instance',onto_pref + '#HomoEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'HomoEnergy_' + calc_id,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_HomoEnergy'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([ comp_pref + 'HomoEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_HomoEnergy'
            , onto_pref + '#hasHomoEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                         comp_pref + 'FloatValue_' + calc_id + '_HomoEnergy',
                         '', data["HOMO energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_HomoEnergy'
                             ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])
    #HOMO-1
    spamwriter.writerow([comp_pref + 'HomoMinusOneEnergy_' + calc_id
                     ,'Instance',onto_pref + '#HomoMinusOneEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'HomoMinusOneEnergy_' + calc_id ,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_HomoMinusOneEnergy'
                         ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([comp_pref + 'HomoMinusOneEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_HomoMinusOneEnergy'
            , onto_pref + '#hasHomoMinusOneEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                        comp_pref + 'FloatValue_' + calc_id + '_HomoMinusOneEnergy',
                         '', data["HOMO-1 energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_HomoMinusOneEnergy'
                            ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])
    #HOMO-2
    spamwriter.writerow([comp_pref + 'HomoMinusTwoEnergy_' + calc_id
                     ,'Instance',onto_pref + '#HomoMinusTwoEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'HomoMinusTwoEnergy_' + calc_id ,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' +  calc_id + '_HomoMinusTwoEnergy'
                         ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([comp_pref + 'HomoMinusTwoEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' +  calc_id + '_HomoMinusTwoEnergy'
             , onto_pref + '#hasHomoMinusTwoEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                         comp_pref + 'FloatValue_' +  calc_id + '_HomoMinusTwoEnergy',
                         '', data["HOMO-2 energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' +  calc_id + '_HomoMinusTwoEnergy'
                            ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])

def write_virt(spamwriter,jobIRI,calc_id,data):
    #This section writes the information on the unoccupied (virtual) orbitals: LUMO, LUMO+1, LUMO+2 energies.
    #LUMO
    spamwriter.writerow([comp_pref + 'LumoEnergy_' + calc_id
                     ,'Instance',onto_pref + '#LumoEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'LumoEnergy_' + calc_id ,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoEnergy'
                         ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([comp_pref + 'LumoEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' + calc_id  + '_LumoEnergy'
             , onto_pref + '#hasLumoEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                        comp_pref + 'FloatValue_' + calc_id  + '_LumoEnergy',
                         '', data["LUMO energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoEnergy'
                             ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])
    #LUMO+1
    spamwriter.writerow([comp_pref + 'LumoPlusOneEnergy_' + calc_id
                     ,'Instance',onto_pref + '#LumoPlusOneEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'LumoPlusOneEnergy_' + calc_id ,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusOneEnergy'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([comp_pref + 'LumoPlusOneEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusOneEnergy'
             , onto_pref + '#hasLumoPlusOneEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                         comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusOneEnergy',
                         '', data["LUMO+1 energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusOneEnergy',
                         'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])
    #LUMO+2
    spamwriter.writerow([comp_pref + 'LumoPlusTwoEnergy_' + calc_id
                     ,'Instance',onto_pref + '#LumoPlusTwoEnergy','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                 comp_pref + 'LumoPlusTwoEnergy_' + calc_id ,
                 gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusTwoEnergy'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
    spamwriter.writerow([comp_pref + 'LumoPlusTwoEnergy_' + calc_id
             ,'Instance',comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusTwoEnergy'
             , onto_pref + '#hasLumoPlusTwoEnergy','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                        comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusTwoEnergy',
                         '', data["LUMO+2 energy"],''])
    spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id  + '_LumoPlusTwoEnergy'
                             ,'Instance',unit_pref + 'unit#Hartree',gain_pref + 'hasUnit','',''])

def write_geom_opt(spamwriter,jobIRI,calc_id,data):
    #This section writes the geometry optimization, spin multiplicity and formal charge information.
    spamwriter.writerow([comp_pref + 'GeometryOptimization_' + calc_id
                    ,'Instance',gain_pref + 'GeometryOptimization','','',''])
    spamwriter.writerow([jobIRI,'Instance',
                comp_pref + 'GeometryOptimization_' + calc_id,
                gain_pref +'isCalculationOn','',''])
    spamwriter.writerow([comp_pref + 'Molecule_' + calc_id
                ,'Instance',gain_pref + 'Molecule','','',''])
    spamwriter.writerow([comp_pref + 'GeometryOptimization_' + calc_id
                    ,'Instance',comp_pref + 'Molecule_' + calc_id ,
                    gain_pref + 'hasMolecule','',''])
    spamwriter.writerow([onto_pref + '#hasSpinMultiplicity','Data Property',
                        comp_pref + 'Molecule_' + calc_id,
                        '',data["Spin multiplicity"],''])
    spamwriter.writerow([comp_pref + 'IntegerValue_' + calc_id + '_FormalCharge'
            ,'Instance',gain_pref + 'IntegerValue','','',''])
    spamwriter.writerow([comp_pref + 'Molecule_' + calc_id
            ,'Instance',comp_pref + 'IntegerValue_' + calc_id + '_FormalCharge'
            ,gain_pref + 'hasFormalCharge','',''])
    spamwriter.writerow([gain_pref + 'hasValue','Data Property',comp_pref + 'IntegerValue_' + calc_id + '_FormalCharge',
                            '',data["Formal charge"],''])
    spamwriter.writerow([comp_pref + 'IntegerValue_' + calc_id + '_FormalCharge',
                            'Instance',gain_pref + 'atomicUnit',gain_pref + 'hasUnit','',''])

def write_atom_info(spamwriter,calc_id,data):
    #This section writes the atom coordinates and masses information.
    count = 1 #This count essentially counts the indices of the atoms starting with 1. Basically, this additional number helps uniquely assign an IRI to each atom.
    coord_string = ['x3','y3','z3'] #How the coordinates are labeled.
    coords = ['X','Y','Z'] #The three cartesian corrdinates.
    #Coordinates.
    for k in range(len(data["Atom types"])):
        spamwriter.writerow([comp_pref + 'Atom_'+ calc_id + '_' + data["Atom types"][k] + str(count)
                        ,'Instance',gain_pref + 'Atom','','',''])
        spamwriter.writerow([comp_pref + 'Molecule_' + calc_id
                ,'Instance',comp_pref + 'Atom_'+ calc_id + '_' + data["Atom types"][k] + str(count)
                ,gain_pref + 'hasAtom','',''])
        spamwriter.writerow([comp_pref + 'Atom_' + calc_id + '_' + data["Atom types"][k] + str(count)
                    ,'Instance',table_pref + '#' + data["Atom types"][k],gain_pref + 'isElement','',''])
        for i in range(3):
            spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k] + str(count) + '_' + coord_string[i] + 'Coordinate'
                                    , 'Instance',gain_pref + 'FloatValue',
                                    '','',''])
            spamwriter.writerow([comp_pref + 'Atom_' + calc_id + '_' + data["Atom types"][k] + str(count)
                        ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k] + str(count) + '_' + coord_string[i] + 'Coordinate',
                                    gain_pref + 'hasAtomCoordinate' + coords[i],'',''])
            spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                                    comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k] + str(count) + '_' + coord_string[i] + 'Coordinate'
                                    ,'',data["Geometry"][k][i],''])
        #Write atom masses.
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k]  + str(count) + '_Mass'
                        ,'Instance',gain_pref + 'FloatValue','','',''])
        spamwriter.writerow([comp_pref + 'Atom_' + calc_id + '_' + data["Atom types"][k] + str(count)
                        ,'Instance',comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k]  + str(count) + '_Mass'
                        ,gain_pref + 'hasMass','',''])
        spamwriter.writerow([gain_pref + 'hasValue','Data Property',
                            comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k]  + str(count) + '_Mass',
                                '',data["Atomic masses"][k],''])
        spamwriter.writerow([comp_pref + 'FloatValue_' + calc_id + '_' + data["Atom types"][k]  + str(count) + '_Mass'
                        ,'Instance',unit_pref + 'unit#Dalton',gain_pref + 'hasUnit','',''])
        count +=1

def write_metadata(spamwriter,calc_id,data):
    #These are the final parts of the ABox with the auxillary info like software used and job run date.
    spamwriter.writerow([onto_pref + '#hasProgram' ,'Data Property',
                            comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                    ,'',data["Program name"],''])
    spamwriter.writerow([onto_pref + '#hasProgramVersion' ,'Data Property',
                            comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                    ,'',data["Program version"].split('+')[0][-1],''])
    spamwriter.writerow([onto_pref + '#hasRunDate' ,'Data Property',
                        comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                ,'',data["Run date"],''])
    spamwriter.writerow([data_pref + 'OutputSource_' + calc_id + '.g09' ,'Instance', onto_pref + '#OutputSource'
            ,'','',''])
    spamwriter.writerow([comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                        ,'Instance',data_pref + 'OutputSource_' + calc_id + '.g09' ,gain_pref + 'hasOutputFile','',''])
    spamwriter.writerow([data_pref + 'OutputSource_' + calc_id + '.xml' ,'Instance', onto_pref + '#OutputSource'
        ,'','',''])
    spamwriter.writerow([comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                        ,'Instance',data_pref + 'OutputSource_' + calc_id + '.xml',gain_pref + 'hasOutputFile','',''])
    spamwriter.writerow([data_pref + 'OutputSource_' + calc_id + '.png','Instance', onto_pref + '#OutputSource'
    ,'','',''])
    spamwriter.writerow([comp_pref + 'SourcePackage_' + calc_id + '_EnvironmentModule'
                        ,'Instance',data_pref + 'OutputSource_' + calc_id + '.png' ,gain_pref + 'hasOutputFile','',''])
