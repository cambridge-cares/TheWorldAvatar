##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 21 Dec 2021                      #
##########################################

import property_reader as pr

"""This module is designed to upload ABox and TBox files to the knowledge graph."""

def upload_ontology(triple_store_url, namespace, ontology_file_path):
    """
    This function uploads an ontology to a triple store.
    """
    print('test function')

def upload_gas_grid_ontologies():
    """
    This function uploads all ontologies required for the Gas Grid Agent to the knowledge graph.
    """
    upload_ontology(pr.getTripleStoreURL, pr.getTripleStoreNamespace, pr.getGridComponentABoxFilePath)
    upload_ontology(pr.getTripleStoreURL, pr.getTripleStoreNamespace, pr.getPipelineABoxFilePath)

if __name__ == '__main__':
    """Calls the RDF conversion function"""
    upload_gas_grid_ontologies()
