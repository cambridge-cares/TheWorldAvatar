from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.querytemplates import *
from pubchemagent.kgoperations.getkgdata import *
import uuid
import re
   
# a sample data addition function
def insert_ontospecies_identifiers(uuid, identifiers):
    for item in identifiers:
        insert_str = pubchem_identifiers_insert(uuid, identifiers[item])
        sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)

def insert_ontospecies_props(uuid, props):
    for item in props:
        insert_str = pubchem_prop_insert(uuid, props[item])
        sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)

def insert_ontospecies_element(uuid, data):
    prev_key = ''
    for item in data:
        if data[item].get('key') == prev_key:
            i = i+1 
        else:
            i = 1
        if data[item].get('reference'):
            prov_uuid = provenance(data[item].get('reference'))
        if data[item].get('type')=='identifier':
            insert_str = pubchem_elem_id_insert(uuid, i,  prov_uuid, data[item])
        elif data[item].get('type') in {'num_prop', 'thermo_prop'}:
            if 'unit' in data[item].get('value'):
                unit_string = str(data[item].get('value').get('unit'))
                unit_uuid = unit(unit_string)
                if data[item].get('type')=='num_prop':
                    insert_str = pubchem_elem_num_prop_insert(uuid, i, prov_uuid, unit_uuid, data[item])
                elif data[item].get('type')=='thermo_prop':
                    ref_unit_string = str(data[item].get('value').get('ref_unit'))
                    ref_unit_uuid = unit(ref_unit_string)
                    insert_str = pubchem_elem_thermo_prop_insert(uuid, i, prov_uuid, unit_uuid, ref_unit_uuid, data[item])
        elif data[item].get('type') == 'string_prop':
            insert_str = pubchem_elem_string_prop_insert(uuid, i, prov_uuid, data[item])
        elif data[item].get('type') == 'classification':
            typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#' + data[item].get('key') + '>'
            classification_uuid = find_uuid(data[item].get('key'), typeIRI, data[item].get('description'))
            insert_str = pubchem_elem_classification_insert(uuid, i, prov_uuid, classification_uuid, data[item])

        prev_key = data[item].get('key')

        sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
        # create a SPARQL object for performing the query
        kg_client = kg_operations(sparqlendpoint)
        kg_client.insertkg(insertStr=insert_str)

def provenance(prov_string):
        IRI = get_provenance_data(prov_string)
        if IRI:
            uuid = IRI.partition('_')[2]
        else:
            uuid = create_uuid()
            insert_str = provenance_insert(uuid, prov_string)
            sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid

def unit(unit_string):
        unit_string=re.sub("°","deg",unit_string)
        IRI = get_unit_data(unit_string)
        if IRI:
            uuid = IRI.partition('_')[2]
        else:
            uuid = create_uuid()
            insert_str = unit_insert(uuid, unit_string)
            sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid

def find_uuid(name, typeIRI, string):
        IRI = get_uuid(typeIRI, string)
        if IRI:
            uuid = IRI.partition('_')[2]
        else:
            uuid = create_uuid()
            insert_str = generic_insert(name, typeIRI, uuid, string)
            sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid
 
# create a new UUID
def create_uuid():
    return str(uuid.uuid4())