'''
This module contains utility function of config parser
'''

import configparser
import os
from pathlib import Path
from typing import List

from data_classes.ts_data_classes import PropertiesFileProtytype


def write_java_properties_conf(props: dict, outpath: str):
    parser = configparser.ConfigParser()
    parser['top'] = props
    text = '\n'.join(['='.join(item) for item in parser.items('top')])
    with open(outpath, 'w') as config_file:
        config_file.write(text)


def create_property_file(name, db_url, db_user, db_pw, kg_url) -> str:
    outdir = (os.path.join(Path(__file__).parent.parent, 'resources'))
    Path(outdir).mkdir(parents=True, exist_ok=True)
    outpath = os.path.abspath(os.path.join(outdir, "{}.properties".format(name)))
    props = PropertiesFileProtytype.copy()
    props['db.url'] = db_url
    props['db.user'] = db_user
    props['db.password'] = db_pw
    props['sparql.query.endpoint'] = kg_url
    props['sparql.update.endpoint'] = kg_url
    write_java_properties_conf(props, outpath)
    return outpath
