'''
This module contains utility function of config parser
'''

import configparser
import os
from pathlib import Path
from typing import List

from data_classes.ts_data_classes import PropertiesFileProtytype


def load_conf(fullfilepath: str) -> configparser.ConfigParser:
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

'''
Remove sector name and put conf key-value into dict
'''
def conf_to_dict(conf) -> dict:
    conf_dict = {}
    for sector in conf:
        for key in conf[sector]:
            conf_dict[key] = conf[sector][key]
    return conf_dict



def match_properties(props_java: dict, props_ini: dict) -> dict:
    updated = props_java.copy()
    for k in props_java:
        short_k = k.split('.')[-1]
        if short_k in props_ini:
            updated[k] = props_ini[short_k]
    return updated


def write_java_properties_conf(props: dict, outpath: str):
    parser = configparser.ConfigParser()
    parser['top'] = props
    text = '\n'.join(['='.join(item) for item in parser.items('top')])
    with open(outpath, 'w') as config_file:
        config_file.write(text)

# Generate Java property files to call TSClient for each TS instance (each needs one as sparql eps are different)
def create_property_files(base_conf, data_confs) -> dict:
    outdir_name = base_conf['paths']['resourcesDir']
    sqlcfg = base_conf['rdb_access']
    outpaths = {}
    outdir = (os.path.join(Path(__file__).parent.parent, outdir_name))
    Path(outdir).mkdir(parents=True, exist_ok=True)
    for datacfg in data_confs:
        dataname = datacfg['source']['name']
        props = PropertiesFileProtytype.copy()
        allcfg = dict(datacfg['kg_access']).copy()
        allcfg.update(dict(sqlcfg))
        updated_props = match_properties(props, allcfg)
        outpath = os.path.abspath(os.path.join(outdir, "{}.properties".format(dataname)))
        write_java_properties_conf(updated_props, outpath)
        outpaths[dataname] = outpath
    return outpaths

def create_property_file(name, db_url, db_user, db_pw, kg_url ) -> str:
    outdir = (os.path.join(Path(__file__).parent.parent, 'resources'))
    Path(outdir).mkdir(parents=True, exist_ok=True)
    outpath = os.path.abspath(os.path.join(outdir, "{}.properties".format(name)))
    props = PropertiesFileProtytype.copy()
    props['db.url'] = db_url
    props['db.user'] = db_user
    props['db.password'] = db_pw
    props['sparql.query.endpoint']  =kg_url
    props['sparql.update.endpoint']  =kg_url
    write_java_properties_conf(props, outpath)
    return outpath