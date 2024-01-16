import os
from Algorithms import alg1_kg
from Algorithms import alg2_kg
from Algorithms import create_precursors_kg
import Algorithms.instantiation_kg as inst
from pyderivationagent import PySparqlClient

import os
dir_path = os.path.dirname(os.path.realpath(__file__))

# Define SPARQL endpoint
SPARQL_ENDPOINT = 'http://localhost:48082/blazegraph/namespace/kb/sparql'

# Define file paths
INPUT_PATH = os.path.join(dir_path, 'Data', 'InputData')
OUTPUT_PATH = os.path.join(dir_path, 'Data', 'OutputDataFromKG')
PRECURSOR_FILE = os.path.join(INPUT_PATH, 'Precursor.csv')
REACTION_FILE = os.path.join(INPUT_PATH, 'Reactions.csv')
OCN_FILE = os.path.join(INPUT_PATH, 'BS.csv')
BS_SMARTS_FILE = os.path.join(INPUT_PATH, 'BS_SMARTS.csv')
CORE_SMARTS_FILE = os.path.join(INPUT_PATH, 'CORE_SMARTS.csv')
TTL_FILE = os.path.join(OUTPUT_PATH, 'instantiation.ttl')
ALG1_KG_OUTPUT_FILE = os.path.join(OUTPUT_PATH, 'alg1.csv')
ALG2_KG_OUTPUT_FILE = os.path.join(OUTPUT_PATH, 'alg2.csv')
MOL_FILE_DIR = os.path.join(OUTPUT_PATH, 'mol_files')


if __name__ == '__main__':
    if not os.path.exists(OUTPUT_PATH):
        os.makedirs(OUTPUT_PATH)
    if not os.path.exists(MOL_FILE_DIR):
        os.makedirs(MOL_FILE_DIR)

    # Instantiation all triples from provided CSV files
    sparql_client = PySparqlClient(
        query_endpoint=SPARQL_ENDPOINT,
        update_endpoint=SPARQL_ENDPOINT,
    )
    sparql_client.performUpdate('delete where {?s ?p ?o}')
    g, bs_iri_dict, core_iri_dict = inst.instantiate_precursor_lfr_ocn(
        PRECURSOR_FILE, REACTION_FILE, OCN_FILE, BS_SMARTS_FILE, CORE_SMARTS_FILE, TTL_FILE,
    )
    sparql_client.uploadGraph(g)
    print(f"Number of base Precursors: {inst.number_of_precursor(sparql_client)}")

    # Run Algorithm 2, including all hypothetical ones
    alg2_kg.expand_precursor_space_lfr(sparql_client, ALG2_KG_OUTPUT_FILE, True)
    print(f"Number of Precursors after alg2 expansion from base: {inst.number_of_precursor(sparql_client)}")

    # Run Algorithm 1, including all hypothetical ones
    alg1_kg.expand_precursor_space(sparql_client, ALG1_KG_OUTPUT_FILE, True)
    print(f"Number of Precursors after alg1 expansion from the expanded space by alg2: {inst.number_of_precursor(sparql_client)}")

    # Run precursor creation
    create_precursors_kg.create_mol_for_all_precursors(sparql_client, MOL_FILE_DIR)

    print('All done.')
