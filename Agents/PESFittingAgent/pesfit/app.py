from pesfit.kgoperations.getkgdata import get_kg_data
from pesfit.jobsubmission.submitslurm import submitslurm
from pesfit.jobsubmission.create_input_folder import create_input_folder
import os
import shutil

def pesfit_wrapper(args):

    onto_pes_scan_iri = args['--opesIRI']
    conf_file=args['--conf-file']

    # remove all files in the working directory
    for f in os.listdir():
        try:
            if f!=conf_file and (os.path.isfile(f) or os.path.islink(f)):
                os.unlink(f)
            elif os.path.isdir(f):
                shutil.rmtree(f)
        except Exception as e:
            print('Failed to delete %s. Reason: %s' % (f, e))

    [state1, state2] = get_kg_data(onto_pes_scan_iri)
    create_input_folder(state1, state2, conf_file)
    submitslurm(conf_file)