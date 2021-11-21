import argparse
import collections
import json
import logging
import logging.config
import os
import pickle
import pprint
import random
import sys
import yaml

import owlready2
import rdflib

import numpy as np
import pandas as pd

import ontomatch.utils.blackboard
from ontomatch.utils.ontologyWrapper import Ontology

def init_file_logging(log_config_file, log_file):
    print('initializing logging with log config file=', log_config_file, ', log file=', log_file)
    with open(log_config_file, 'r') as file:
        # always use safe_load to avoid reading and executing as YAML serialized Python code
        # yaml returns a dictionary
        log_cfg = yaml.safe_load(file.read())

    pprint_log_cfg = pprint.PrettyPrinter().pformat(log_cfg)
    print(pprint_log_cfg)

    if log_file:
        log_cfg['handlers']['file_handler']['filename'] = log_file
    else:
        log_file = log_cfg['handlers']['file_handler']['filename']

    dir_name = os.path.dirname(log_file)
    try:
        os.makedirs(dir_name, exist_ok=True)
    except FileExistsError:
        print('dir already exists, dir=', dir_name)

    # use logging configuration with dictionary
    logging.config.dictConfig(log_cfg)

    logging.info(concat('initialized logging with config file=', log_config_file, ', log file=', log_file))

def init_logging(log_conf_dir='./conf', log_dir='../logs'):
    # file and console logging with Python's standard logging library
    log_config_file = log_conf_dir + '/logging.yaml'
    name = 'ontomatch'
    init_file_logging(log_config_file, log_dir + '/' + name + '.log')

def read_json_from_path(path:str) -> dict:
    with open(path) as json_file:
        config = json.load(json_file, object_pairs_hook=collections.OrderedDict)
    return config

def convert_json_to_dict(json_str:str) -> dict:
    return json.loads(json_str, object_pairs_hook=collections.OrderedDict)

def init(config_dev=None):
    print('current working directory=', os.getcwd())
    print('sys.argv=', sys.argv)

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', type=str)
    parser.add_argument('--logconfdir', type=str, default='./conf')
    parser.add_argument('--logdir', type=str, default='../logs')
    args = parser.parse_args()
    print('args = ', args)

    if args.config:
        config = read_json_from_path(args.config)
    else:
        config = config_dev

    init_logging(args.logconfdir, args.logdir)
    logging.info('current working directory=%s', os.getcwd())
    logging.info('args=%s', args)
    logging.info('config=%s', config)
    try:
        seed = config['numerical_settings']['seed']
    except TypeError:
        seed = 1
    logging.info('setting random seed=%s', seed)
    np.random.seed(seed)
    random.seed(seed)

    return config, args.config

def concat(*args):
    if len(args) == 1:
        return args[0]
    else:
        message = ''
        for m in args:
            message += str(m) + ' '
        return message

def log(*args):
    logging.info(concat(*args))

def logm(*args):
    logging.getLogger().info(args)

def get_prop_columns(dframe):
    columns = []
    for c in dframe.columns:
        if not (isinstance(c, int) or c.endswith('_max')):
            try:
                int(c)
            except ValueError:
                continue
        columns.append(c)
    return columns

def read_csv(file):
    dframe = pd.read_csv(file)
    dframe['idx_1'] = dframe['idx_1'].astype(str)
    dframe['idx_2'] = dframe['idx_2'].astype(str)
    fct = lambda s : s.replace('http://www.google.com/base/feeds/snippets/', '')
    dframe['idx_2'] = dframe['idx_2'].apply(fct)
    dframe.set_index(['idx_1', 'idx_2'], inplace=True)
    return dframe

def serialize_graph_to_str(graph: rdflib.Graph, frmt = 'turtle') -> str:
    # https://rdflib.readthedocs.io/en/stable/apidocs/rdflib.html:
    # If encoding is None and destination is None, returns a string If encoding is set, and Destination is None, returns bytes
    # this doesn't work correctly: when writing graph_str to file, an error is raised
    graph_bytes = graph.serialize(destination=None, encoding=None, format=frmt)
    graph_str = graph_bytes.decode('utf-8')
    return graph_str

def parse_graph(addr) -> rdflib.Graph:
    frmt = 'xml'
    if addr.endswith('.ttl'):
        frmt = 'turtle'
    graph = rdflib.Graph()
    graph.parse(addr, format=frmt)
    return graph

def call_agent_blackboard_for_writing(addr:str, serialized_object:str, http:bool=False) -> str:
    logging.info('calling blackboard for writing, addr=%s', addr)
    if http:
        raise NotImplementedError()
    else:
        handle = ontomatch.utils.blackboard.Agent().write(addr, serialized_object)
    logging.info('called blackboard for writing, handle=%s', handle)
    return handle

def call_agent_blackboard_for_reading(handle:str, http:bool=False) -> str:
    logging.info('calling blackboard for reading, handle=%s, http=%s', handle, http)
    if http:
        raise NotImplementedError()
    else:
        object = ontomatch.utils.blackboard.Agent().read(handle)
    logging.info('called blackboard for reading')
    return object

def call_agent_blackboard_for_upload(name:str, object_for_upload:object, do_pickle=False, http:bool=False) -> str:
    logging.info('calling blackboard for reading, handle=%s, http=%s', handle, http)
    if http:
        raise NotImplementedError()
    else:
        serialized_object = ontomatch.utils.blackboard.Agent().upload(name, upload_from)
    logging.info('called blackboard for reading')
    return serialized_object

def load_ontology(graph_handle, blackboard=True):
    logging.info('loading ontology for %s', graph_handle)

    if graph_handle.endswith('.pkl'):
        with open(graph_handle,'rb') as file:
            onto = pickle.load(file)
    else:
        if blackboard:
            file = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/' + graph_handle
        else:
            file = graph_handle
        graph = ontomatch.utils.util.parse_graph(file)

        if file.endswith('.owl') or graph_handle.endswith('xml'):
            owlready2onto = owlready2.get_ontology(file).load()
        else:
            # This is a hack to convert the rdflib graph into owlready2
            # unfortunately, owlready2 only allows an URL or file name as parameter for loading, no stream parameter
            tmp_file = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/tmp_rdflib_onto_for_owlready2.owl'
            graph.serialize(tmp_file, format="xml")
            owlready2onto = owlready2.get_ontology(tmp_file).load()

        onto = Ontology(graph_handle, ontology=owlready2onto, graph=graph, skip_labels=False)

    logging.info('finished loading ontology for %s', graph_handle)
    return onto

def pickle_dump(addr, onto):
    onto.ontology = None
    onto.graph = None
    pklname = addr.replace('rdf','pkl').replace('owl','pkl').replace('xml','pkl').replace('ttl', 'pkl')
    logging.info('dumping ontology to file=%s', pklname)
    with open(pklname,'wb') as file:
        pickle.dump(onto, file, -1)