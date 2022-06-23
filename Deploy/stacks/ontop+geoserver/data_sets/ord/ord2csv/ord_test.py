# Import modules
from email.encoders import encode_noop
from encodings import utf_8
import ord_schema
from ord_schema import message_helpers, validations
from ord_schema.proto import dataset_pb2

import math
import pandas as pd
import numpy as np
from requests import head
import tensorflow as tf
import matplotlib.pyplot as plt
import os
import wget

from rdkit import Chem
from rdkit.Chem import AllChem
from sklearn import model_selection, metrics
from glob import glob

import csv

# Download dataset from ord-data

url = "https://github.com/open-reaction-database/ord-data/blob/main/data/1b/ord_dataset-1b3d2b114de1429e9b70c3b1c16c9263.pb.gz?raw=true"
# pb = wget.download(url)
# pb = 'ord_dataset-1b3d2b114de1429e9b70c3b1c16c9263.pb.gz'

# Deoxyfluorinatoin data
url = "https://github.com/open-reaction-database/ord-data/blob/main/data/fc/ord_dataset-fc83743b978f4deea7d6856deacbfe53.pb.gz?raw=true"
# pb = wget.download(url)
pb = 'ord_dataset-fc83743b978f4deea7d6856deacbfe53.pb.gz'


# Load Dataset message
data = message_helpers.load_message(pb, dataset_pb2.Dataset)

message_helpers.write_message(data, 'test.json')


# Function for processing reaction identifier
ReactionIdentifierType = {0:'UNSPECIFIED',
                  1:'CUSTOM',
                  2:'REACTION_SMILES',
                  6:'REACTION_CXSMILES',
                  3:'RDFILE',
                  4:'RINCHI',
                  5:'NAME'}
def process_reaction_identifier(identifer):
    return identifer.type, identifer.details, identifer.value, identifier.is_mapped


# Compound identifier retriever
def get_compound_identifiers(compound):
    identifiers = [None] * 2 * 17
    for identifier in compound.identifiers:
        identifiers[identifier.type*2:identifier.type*2+2] = [identifier.details, identifier.value]
    return identifiers  

def get_compound_identifiers(component):
    identifiers = [None] * 2 * 17
    for identifier in component.identifiers:
        identifiers[identifier.type*2:identifier.type*2+2] = [identifier.details, identifier.value]
    return identifiers

mass_unit = {0 : 'UNSPECIFIED',
             1 : 'KILOGRAM',
             2 : 'GRAM',
             3 : 'MILLIGRAM',
             4 : 'MICROGRAM'}

moles_unit = {0 : 'UNSPECIFIED',
              1 : 'MOLE',
              2 : 'MILLIMOLE',
              3 : 'MICROMOLE',
              4 : 'NANOMOLE'}
volume_unit = {0 : 'UNSPECIFIED',
               1 : 'LITER',
               2 : 'MILLILITER',
               3 : 'MICROLITTER',
               4 : 'NANOLITER'}
unmeasured_amount = {0 : 'UNSPECIFIED',
                     1 : 'CUSTOM',
                     2 : 'SATURATED',
                     3 : 'CATALYTIC',
                     4 : 'TITRATED'}
reaction_role = {0 : 'UNSPECIFIED',
                 1 : 'REACTANT',
                 2 : 'REAGENT',
                 3 : 'SOLVENT',
                 4 : 'CATALYST',
                 5 : 'WORKUP',
                 6 : 'INTERNAL_STANDARD',
                 7 : 'AUTHENTIC_STANDARD',
                 8 : 'PRODUCT'}


def get_compound_amount(component, mass_unit, moles_unit, volume_units, unmeasured_amount):
    amount = [None] * (3*3 + 2 +1)
    amount[0] = component.amount.mass.value
    amount[1] = component.amount.mass.precision
    amount[2] = mass_unit[component.amount.mass.units]
    amount[3] = component.amount.moles.value
    amount[4] = component.amount.moles.precision
    amount[5] = moles_unit[component.amount.moles.units]
    amount[6] = component.amount.volume.value
    amount[7] = component.amount.volume.precision
    amount[8] = volume_unit[component.amount.volume.units]
    amount[9] = unmeasured_amount[component.amount.unmeasured.type]
    amount[10] = component.amount.unmeasured.details
    amount[11] = component.amount.volume_includes_solutes
 

    return amount

def get_prepration(component):
    preps = [None] * 2 * 7
    for preparation in component.preparations:
        preps[preparation.type*2:preparation.type*2+2] = [preparation.details, preparation.reaction_id]
    return preps

def get_feature_analyses_keys(component):
    keys = [None] * 2
    for feature in component.features:
        keys[0] = feature
    for analysis in component.analyses:
        keys[1] = analysis
    return keys



# reaction_id
e = open('reaction_id.csv', 'w', newline='', encoding='utf-8')
# reaction_identifiers
f = open('reaction_identifier.csv', 'w', newline='', encoding='utf-8')
# reaction_inputs
g = open('reaction_inputs.csv', 'w', newline='', encoding='utf-8')
# reaction_compounds
h = open('reaction_compounds.csv', 'w', newline='', encoding = 'utf-8')

writer_e = csv.writer(e)
header = ['reaction_id']
writer_e.writerow(header)
writer_f = csv.writer(f)
header = ['reaction_id', 'type', 'details', 'value', 'is_mapped']
writer_f.writerow(header)
writer_g = csv.writer(g)
header = ['reaction_id','input_description',
          'crude_components_reaction_id', 'crude_components_include_workups', 'crude_component_has_derived_amount', 'crude_component_amount',  
          'addition_order', 
          'addition_time_value', 'addition_time_precision', 'addition_time_units',
          'addition_speed_type', 'addition_speed_details',
          'addition_duration_value', 'addition_duration_precision', 'addition_duration_units',
          'flow_rate_value', 'flow_rate_precision', 'flow_rate_units',
          'addition_device_type', 'addition_device_details',
          'addition_temperature_value', 'addition_temperature_precision', 'addition_temperature_units']

writer_g.writerow(header)
writer_h = csv.writer(h)
header = ['reaction_id', 'input_description', 
          'identifier_unspecified_detail', 'identifer_unspecified_value',
          'identifier_custom_detail', 'identifer_custom_value',
          'identifier_smiles_detail', 'identifer_smiles_value',
          'identifier_inchi_detail', 'identifer_inchi_value',
          'identifier_molblock_detail', 'identifer_molblock_value',
          'identifier_iupac_name_detail', 'identifer_iupac_name_value',
          'identifier_name_detail', 'identifer_name_value',
          'identifier_cas_number_detail', 'identifer_cas_number_value',
          'identifier_pubchem_cid_detail', 'identifer_pub_cid_value',
          'identifier_chemspider_id_detail', 'identifer_chemspider_id_value',
          'identifier_cxsmiles_detail', 'identifer_cxsmiles_value',
          'identifier_inchi_key_detail', 'identifer_inchi_key_value',
          'identifier_xyz_detail', 'identifer_xyz_value',
          'identifier_uniport_id_detail', 'identifer_uniport_id_value',
          'identifier_pdb_id_detail', 'identifer_pdb_id_value',
          'identifier_amino_acid_sequence_detail', 'identifer_amino_acid_sequence_value',
          'identifier_helm_detail', 'identifer_helm_value',
          'amount_mass', 'amount_mass_precision', 'amount_mass_units', 
          'amount_moles', 'amount_moles_precision', 'amount_moles_units',
          'amount_volume', 'amount_volume_precision', 'amount_volume_units',
          'amount_unspecified', 'amount_unspecified_details', 
          'amount_volume_includes_solutes',
          'reaction_role','is_limiting',
          'prepreation_unspecified_details', 'prepreation_unspecified_reaction_id',
          'prepreation_custom_details', 'prepreation_custom_reaction_id',
          'prepreation_none_details', 'prepreation_none_reaction_id',
          'prepreation_repurified_details', 'prepreation_repurified_reaction_id',
          'prepreation_sparged_details', 'prepreation_sparged_reaction_id',
          'prepreation_dried_details', 'prepreation_dried_reaction_id',
          'prepreation_synthesized_details', 'prepreation_synthesized_reaction_id',
          'source_vendor', 'source_id', 'source_lot',
          'feature','analyses']
writer_h.writerow(header)




for reaction in data.reactions:

    for identifier in reaction.identifiers:
        type, details, value, is_mapped = process_reaction_identifier(identifier)
        reaction_identifier = [reaction.reaction_id, ReactionIdentifierType[type], details, value, is_mapped]
        writer_f.writerow(reaction_identifier)


         

    for input in reaction.inputs:
        component = [reaction.reaction_id, input]
        try:
            crude_components = [reaction.inputs[input].crude_components.reaction_id,
                                reaction.inputs[input].crude_components.includes_workup,
                                reaction.inputs[input].crude_components.has_derived_amount,
                                reaction.inputs[input].crude_components.amount]
        except:
            crude_components = [None] * 4
        try:
            addition_order = [reaction.inputs[input].addition_order]
        except:
            addition_order = [None]
        try:
            addition_time = [reaction.inputs[input].addion_time.value, reaction.inputs[input].addion_time.precision, reaction.inputs[input].addion_time.units ]
        except:
            addition_time = [None] * 3
        try:
            addition_speed = [reaction.inputs[input].addition_speed.type, reaction.inputs[input].addition_speed.value]
        except:
            addition_speed = [None] * 2
        try:
            addition_duration = [reaction.inputs[input].addition_duration.value, reaction.inputs[input].addition_duration.precision, reaction.inputs[input].addition_duration.units]
        except:
            addition_speed = [None] * 3
        try:
            flow_rate = [reaction.inputs[input].flow_rate.value, reaction.inputs[input].flow_rate.precision, reaction.inputs[input].flow_rate.units]
        except:
            flow_rate = [None] * 3
        try:
            addition_device = [reaction.inputs[input].addition_device.type, reaction.inputs[input].addition_device.details]
        except:
            addition_device = [None] * 2
        try:
            addition_temperature = [reaction.inputs[input].addition_tempurature.value, reaction.inputs[input].addition_tempurature.precision, reaction.inputs[input].addition_tempurature.units]
        except:
            addition_temperature = [None] * 3
        writer_g.writerow(component+crude_components + addition_order + addition_time + addition_speed + flow_rate + addition_device + addition_temperature)        
    
    
        comp_count = 1

        for component in reaction.inputs[input].components :

           id = get_compound_identifiers(component)
           amnt = get_compound_amount(component, mass_unit, moles_unit, volume_unit, unmeasured_amount) 
           prep = get_prepration(component)   
           source = [component.source.vendor, component.source.id, component.source.lot]  
           feature_analyses = get_feature_analyses_keys(component)
            
           writer_h.writerow([reaction.reaction_id, input] + id + amnt + [reaction_role[component.reaction_role], component.is_limiting]\
                 + prep + source + feature_analyses)
           comp_count += 1

e.close()
f.close()
g.close()
h.close()
    # print(reaction.reaction_id, type, details, value, is_mapped)
    # for identifier in reaction.identifiers:
        # reaction_identifiers = process_reaction_identifier(identifier)         
        # print(reaction_identifiers.type, reaction_identifiers.details, reaction_identifiers.value, reaction_identifiers.is_mapped)
        # print(identifier.type, identifier.details, identifier.value, identifier.is_mapped)