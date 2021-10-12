from pesfit.kgoperations.getkgdata import get_kg_data
import os

def pesfit_wrapper(args):

    # remove all files in the working directory
    for f in os.listdir():
        os.remove(f)   

    onto_pes_scan_iri = args['<ONTO_PES_SCAN_IRI>']
    get_kg_data(onto_pes_scan_iri)
    