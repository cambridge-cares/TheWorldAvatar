import os
import shutil
import glob
import json

def create_input_folder(state1, state2, conf_file):

    input_path = 'input'
    os.mkdir(input_path)

    dlpathfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\bash_script')) 
    shfiles = glob.glob(dlpathfile + "\*.sh")
    for f in shfiles:
        shutil.copy(f, input_path) 

    create_scan_folder(input_path, conf_file)
    state=[state1, state2]
    create_dl_field_folder(input_path, state)
    create_states_folders(input_path)
    create_mods_folder(input_path)
    create_validation_folder(input_path)

    shutil.make_archive('input', 'zip', input_path)

def create_scan_folder(input_path, conf_file):
    scan_path = os.path.join(input_path, 'scan')
    os.mkdir(scan_path)
    files = glob.glob(os.getcwd() + "\scan*")
    for file in files:
        file_name = os.path.basename(file)
        shutil.move(file, os.path.join(scan_path, file_name)) 
    
    #input_file_path = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\input.json'))
    input_file_path = conf_file
    lib_path = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\tabulated_vdw'))
    with open(input_file_path) as json_file:
        Inputs = json.load(json_file)
    if Inputs['tabulated_potential']=='isoPAHAP':
        shutil.copy(os.path.join(lib_path, 'TABLE_isoPAHAP'), os.path.join(scan_path, 'TABLE'))


def create_dl_field_folder(input_path, state):
    dlfield_path = os.path.join(input_path, 'dl_field')
    os.mkdir(dlfield_path)
    dlpathfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\templates\dl_f_path_template'))
    fin = open(dlpathfile, "rt")
    data = fin.read()
    fin.close()

    for i in [1, 2]:
        os.mkdir(os.path.join(dlfield_path, 'output' + str(i)))

        statename= 'state' + str(i)
        shutil.copy(os.path.join(input_path, 'scan', 'scan_' + str(state[i-1]) + '.xyz'), os.path.join(dlfield_path, 'state' + str(i) + '.xyz'))

        shutil.copy(dlpathfile, os.path.join(dlfield_path, 'dl_f_path_' + str(i))) 
        datatemp = data.replace('<N>', str(i))
        fin = open(os.path.join(dlfield_path, 'dl_f_path_' + str(i)), "wt")
        fin.write(datatemp)
        fin.close()

    dlcontrolfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\templates\state.control_template'))
    fin = open(dlcontrolfile, "rt")
    data = fin.read()
    fin.close()

    for i in [1, 2]:
        shutil.copy(dlcontrolfile, os.path.join(dlfield_path, 'state'  + str(i) + '.control')) 
        datatemp = data.replace('<N>', str(i))
        fin = open(os.path.join(dlfield_path, 'state'  + str(i) + '.control'), "wt")
        fin.write(datatemp)
        fin.close()

def create_states_folders(input_path):
    for i in [1, 2]:
        statef_path = os.path.join(input_path, 'state' + str(i))
        os.mkdir(statef_path)
        controlfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\templates\CONTROL'))
        shutil.copy(controlfile, os.path.join(statef_path, 'CONTROL')) 

def create_mods_folder(input_path):
    modsf_path = os.path.join(input_path, 'mods')
    os.mkdir(modsf_path)
    modswd_path = os.path.join(modsf_path, 'Working_dir')
    os.mkdir(modswd_path)
    modsinfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\templates\MoDS_inputs_template.xml'))
    shutil.copy(modsinfile, os.path.join(modswd_path, 'MoDS_inputs_template.xml')) 

def create_validation_folder(input_path):
    val_path = os.path.join(input_path, 'validation')
    os.mkdir(val_path)
    controlfile = os.path.relpath(os.path.join(os.path.dirname(__file__), '..' , 'resources\\templates\CONTROL_validation'))
    shutil.copy(controlfile, os.path.join(val_path, 'CONTROL')) 

    
