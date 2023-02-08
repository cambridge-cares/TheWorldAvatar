from bioservices import ChEBI
from pubchemagent.kgoperations.querykg import kg_operations
from pubchemagent.kgoperations.addkgdata import create_uuid
from pubchemagent.kgoperations.getkgdata import get_uuid
from pubchemagent.kgoperations.querytemplates import *
from pubchemagent.kgoperations.queryendpoints import SPARQL_ENDPOINTS
import logging

logging.getLogger('urllib3').setLevel(logging.CRITICAL)
logging.getLogger('requests').setLevel(logging.CRITICAL)
logging.getLogger('requests_cache').setLevel(logging.CRITICAL)
logging.getLogger('bioservices').setLevel(logging.CRITICAL)
logging.getLogger('matplotlib').setLevel(logging.CRITICAL)
logging.getLogger('suds').setLevel(logging.CRITICAL)

def get_chebi_tree(chebiID):
    ch = ChEBI()
    res = ch.getCompleteEntity(chebiID)
    child = res['chebiAsciiName']
    if 'OntologyParents' in res:
        for item in res['OntologyParents']:
            if item['type'] == 'is a':
                chebiID = item['chebiId']
                parent = item['chebiName']
                if 'group' in child and 'entity' not in child:
                    typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#FunctionalGroup>'
                    child_uuid, child_flag = find_uuid('FunctionalGroup', typeIRI, child, 'ChEBI classification')
                    tc = 'FunctionalGroup'
                else: 
                    typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#ChemicalClass>'
                    child_uuid, child_flag = find_uuid('ChemicalClass', typeIRI, child, 'ChEBI classification')
                    tc = 'ChemicalClass'
                if 'group' in parent and 'entity' not in parent:
                    typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#FunctionalGroup>'
                    parent_uuid, parent_flag = find_uuid('FunctionalGroup', typeIRI, parent, 'ChEBI classification')
                    tp = 'FunctionalGroup'
                else:
                    typeIRI = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#ChemicalClass>'
                    parent_uuid, parent_flag = find_uuid('ChemicalClass', typeIRI, parent, 'ChEBI classification')
                    tp = 'ChemicalClass'
                print(child + ' (' + tc + ') is a (' + tp + ') ' + parent)
                insert_triple(tc, child_uuid, tp, parent_uuid, 'rdf:type')
                if parent_flag == 'new':
                    get_chebi_tree(chebiID)
            if item['type'] == 'has part' and 'atom' not in item['chebiName']:
                chebiID = item['chebiId']
                parent = item['chebiName']
                if 'group' in parent:
                    print(child + ' (ChemicalClass) has part (FunctionalGroup) ' + parent)
                    typeIRIp = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#FunctionalGroup>'
                    typeIRIc = '<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#ChemicalClass>'
                    parent_uuid, parent_flag = find_uuid('FunctionalGroup', typeIRIp, parent, 'ChEBI classification')
                    child_uuid, child_flag = find_uuid('ChemicalClass', typeIRIc, child, 'ChEBI classification')
                    insert_triple('ChemicalClass', child_uuid, 'FunctionalGroup', parent_uuid, 'os:hasFunctionalGroup')
                    if parent_flag == 'new':
                        get_chebi_tree(chebiID)

def insert_triple(type_child, child_uuid, type_parent, parent_uuid, relation):
    insert_str = insert_child_parent(type_child, child_uuid, type_parent, parent_uuid, relation)
    sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
    # create a SPARQL object for performing the query
    kg_client = kg_operations(sparqlendpoint)
    kg_client.insertkg(insertStr=insert_str)

def find_uuid(name, typeIRI, string, comment = ''):
        IRI = get_uuid(typeIRI, string)
        if IRI:
            uuid = IRI.partition('_')[2]
            return uuid, 'exist'
        else:
            uuid = create_uuid()
            insert_str = generic_insert(name, typeIRI, uuid, string, comment)
            sparqlendpoint = SPARQL_ENDPOINTS['pubchem']
            # create a SPARQL object for performing the query
            kg_client = kg_operations(sparqlendpoint)
            kg_client.insertkg(insertStr=insert_str)
        return uuid, 'new'


def insert_child_parent(type_child, child_uuid, type_parent, parent_uuid, relation):
    insert_str="""
    PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

    INSERT DATA
    {
    <http://www.theworldavatar.com/kg/ontospecies/#type_child#_#child_uuid#> #relation# <http://www.theworldavatar.com/kg/ontospecies/#type_parent#_#parent_uuid#>
    }    
    """.replace('#child_uuid#', child_uuid).replace('#parent_uuid#', parent_uuid).replace('#type_child#', type_child).replace('#type_parent#', type_parent).replace('#relation#', relation)
    return insert_str
