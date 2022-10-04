import os
import shutil
import json

def slurminputs(conf_file):

    slurmfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\Slurm.sh')) 
    shutil.copy(slurmfile, 'Slurm.sh')

    fin = open('Slurm.sh', "rt", newline='\n')
    data = fin.read()
    fin.close()

    input_file_path = conf_file
    with open(input_file_path) as json_file:
        Inputs = json.load(json_file)

    data = data.replace('<DL_FIELD_PATH>', Inputs['dl_field.executable.path'])
    data = data.replace('<DL_FIELD_LIB_PATH>', Inputs['dl_field_lib.folder.path'])
    data = data.replace('<DL_POLY_PATH>', Inputs['dl_poly.executable.path'])
    data = data.replace('<MoDS_PATH>', Inputs['MoDS.executable.path'])
    
    fin = open('Slurm.sh', "wt", newline='\n')
    fin.write(data)
    fin.close()