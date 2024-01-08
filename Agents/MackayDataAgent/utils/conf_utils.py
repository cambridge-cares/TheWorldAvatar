import configparser
import os

def load_conf(fullfilepath) -> configparser.ConfigParser:
    config = configparser.ConfigParser()
    config.read(fullfilepath)
    print(config)
    return config


def load_confs_from_dir(fulldirpath) -> list:
    filepaths = []
    data_conf_list = []
    for filename in os.listdir(fulldirpath):  # Construct lists of config objects for all data points
        if filename.endswith(".cfg"):
            filepaths.append(os.path.join(fulldirpath, filename))
    for confpath in filepaths:
        data_conf_list.append(load_conf(confpath))
    return data_conf_list


def match_properties(props_java: dict, props_ini: dict) -> dict:
    updated = props_java.copy()
    for k in props_java:
        short_k = k.split('.')[-1]
        if short_k in props_ini:
            updated[k] = props_ini[short_k]
    return updated

def write_java_properties_conf(props:dict, outpath:str):
    parser = configparser.ConfigParser()
    parser['top'] = props
    text = '\n'.join(['='.join(item) for item in parser.items('top')])
    with open(outpath, 'w') as config_file:
        config_file.write(text)