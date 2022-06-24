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
def get_compound_identifiers(identifier, compound_identifier):
    identifiers = [None] * 3
    identifiers = [compound_identifier[identifier.type], identifier.details, identifier.value]
    return identifiers

compound_identifier = {0  : 'UNSEPECIFIED',
                       1  : 'CUSTOM',
                       2  : 'SMILES',
                       3  : 'INCHI',
                       4  : 'MOLBLOCK',
                       5  : 'IUPAC_NAME',
                       6  : 'NAME',
                       7  : 'CAS_NUMBER',
                       8  : 'PUBCHEM_CID',
                       9  : 'CHEMSPIDER_ID',
                       10 : 'CXSMILES',
                       11 : 'INCHI_KEY',
                       12 : 'XYZ',
                       13 : 'UNIPORT_ID',
                       14 : 'PDB_ID',
                       15 : 'AMINO_ACID_SEQUENCE',
                       16 : 'HELM'}

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
compound_preparation = {0 : 'UNSPECIFIED',
                       1 : 'CUSTOM',
                       2 : 'NONE',
                       3 : 'REPURIFIED',
                       4 : 'SPARGED',
                       5 : 'DRIED',
                       6 : 'SYNTHESIZED'}



def get_compound_amount(component, mass_unit, moles_unit, volume_units, unmeasured_amount):
    amount = [None] * 7
    if component.amount.mass.value != 0.0:
        amount[0:5] = ['mass',component.amount.mass.value,component.amount.mass.precision,mass_unit[component.amount.mass.units], component.amount.volume_includes_solutes]
        return amount
    elif component.amount.moles.value != 0.0:
        amount[0:5] = ['moles', component.amount.moles.value, component.amount.moles.precision, moles_unit[component.amount.moles.units],component.amount.volume_includes_solutes]
        return amount
    elif component.amount.volume.value != 0.0:
        amount[0:5] = ['volume', component.amount.volume.value, component.amount.volume.precision, volume_unit[component.amount.volume.units],component.amount.volume_includes_solutes]
    else:     
        amount[4:7] = ['unmeasured', component.amount.volume_includes_solutes, unmeasured_amount[component.amount.unmeasured.type], component.amount.unmeasured.details] 

    return amount

def get_compound_preparation(preparation,compound_preparation):
    preparations = [None] * 3
    preparations = [compound_preparation[preparation.type], preparation.details, preparation.reaction_id]
    return preparations

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
header = ['reaction_id', 'input_description', 'component_count',
          'identifier_type', 'identifier_details', 'identifer_value',
          'amount_kind', 'amount_value', 'amount_precision', 'amount_units', 'amount_volume_includes_solutes', 'amount_unmeasured_type', 'amount_unmeasured_details', 
          'reaction_role','is_limiting',
          'preparation_type', 'preparation_details', 'preparation_reaction_id',
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
    
    
        component_count = 1

        for component in reaction.inputs[input].components :
            for identifier in component.identifiers :
                if (component.preparations):
                    for preparation in component.preparations:
                        id = get_compound_identifiers(identifier, compound_identifier)
                        amnt = get_compound_amount(component, mass_unit, moles_unit, volume_unit, unmeasured_amount)
                        prep = get_compound_preparation(preparation, compound_preparation)
                        source = [component.source.vendor, component.source.id, component.source.lot]
                        feature_analyses = get_feature_analyses_keys(component)
                        writer_h.writerow([reaction.reaction_id, input] + [component_count] + id + amnt + [reaction_role[component.reaction_role], component.is_limiting]\
                         + prep + source + feature_analyses)
                else:
                    id = get_compound_identifiers(identifier, compound_identifier)
                    amnt = get_compound_amount(component, mass_unit, moles_unit, volume_unit, unmeasured_amount)
                    prep = get_compound_preparation(preparation, compound_preparation)
                    source = [component.source.vendor, component.source.id, component.source.lot]
                    feature_analyses = get_feature_analyses_keys(component)
                    writer_h.writerow([reaction.reaction_id, input] + [component_count] + id + amnt + [reaction_role[component.reaction_role], component.is_limiting]\
                     + prep + source + feature_analyses)
            component_count += 1                    

e.close()
f.close()
g.close()
h.close()
    # print(reaction.reaction_id, type, details, value, is_mapped)
    # for identifier in reaction.identifiers:
        # reaction_identifiers = process_reaction_identifier(identifier)         
        # print(reaction_identifiers.type, reaction_identifiers.details, reaction_identifiers.value, reaction_identifiers.is_mapped)
        # print(identifier.type, identifier.details, identifier.value, identifier.is_mapped)