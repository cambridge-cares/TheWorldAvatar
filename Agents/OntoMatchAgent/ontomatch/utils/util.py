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

import numpy as np
import owlready2
import pandas as pd
import rdflib
import sklearn
import sklearn.ensemble
import sklearn.metrics.pairwise
import sklearn.model_selection
import sklearn.svm

import ontomatch.utils.blackboard
from ontomatch.httpCaller import httpcaller as httpcaller
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

    logging.info('initialized logging with config file=%s, log file=%s', log_config_file, log_file)

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
    data_path_default = './data'
    parser.add_argument('--datadir', type=str, default=data_path_default)
    args = parser.parse_args()
    print('args = ', args)

    if args.config:
        config = read_json_from_path(args.config)
    else:
        config = config_dev

    init_logging(args.logconfdir, args.logdir)
    logging.info('current working directory=%s', os.getcwd())
    logging.info('args=%s', args)

    if args.datadir != data_path_default:
        logging.info('changing data paths from %s to %s', data_path_default, args.datadir)
        path = config['dataset']['src']
        config['dataset']['src'] = path.replace(data_path_default, args.datadir)
        path = config['dataset']['tgt']
        config['dataset']['tgt'] = path.replace(data_path_default, args.datadir)
        path = config['post_processing']['evaluation_file']
        config['post_processing']['evaluation_file'] = path.replace(data_path_default, args.datadir)

    logging.info('config=%s', config)
    try:
        seed = config['numerical_settings']['seed']
    except TypeError:
        seed = 1
    logging.info('setting random seed=%s', seed)
    np.random.seed(seed)
    random.seed(seed)

    return config, args.config

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

def read_csv(file:str) -> pd.DataFrame:
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
        params = dict(addr = addr, serialized_object = serialized_object)
        handle = httpcaller.caller().callAgent("blackboard", params)

    else:
        handle = ontomatch.utils.blackboard.Agent().write(addr, serialized_object)
    logging.info('called blackboard for writing, handle=%s', handle)
    return handle

def call_agent_blackboard_for_reading(handle:str, http:bool=False) -> str:
    logging.info('calling blackboard for reading, handle=%s, http=%s', handle, http)
    if http:
        params = dict(handle = handle)
        obj = httpcaller.caller().callAgent("blackboard", params, "GET")
    else:
        obj = ontomatch.utils.blackboard.Agent().read(handle)
    logging.info('called blackboard for reading')
    return obj

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

def generate_train_test_sets_due_to_ratio(df_matches, df_candidate_pairs, match_train_size, nonmatch_ratio, prop_columns=None):
    logging.info('splitting, match=%s, candidate_pairs=%s, match_train_size=%s, nonmatch_ratio=%s',
        len(df_matches), len(df_candidate_pairs), match_train_size, nonmatch_ratio)

    # sample from matches
    number_m = int(match_train_size * len(df_matches))
    df_matches['y'] = 1 # 1 means match
    if match_train_size == len(df_matches):
        df_m_train = df_matches
    else:
        df_m_train, _ = sklearn.model_selection.train_test_split(df_matches, train_size=number_m, shuffle=True)

    # sample from nonmatches
    number_n = int(nonmatch_ratio * number_m)
    # case a) only subtract the matching pairs in the training set
    diff = df_candidate_pairs.index.difference(df_m_train.index)
    # case b) substract all matching pairs in the ground truth
    #diff = df_candidate_pairs.index.difference(df_matches.index)
    df_diff = df_candidate_pairs.loc[diff]
    df_diff['y'] = 0 # 0 means nonmatch
    df_n_train, _ = sklearn.model_selection.train_test_split(df_diff, train_size=number_n, shuffle=True)

    len_false_nonmatches = len(df_n_train.index.intersection(df_matches.index))

    df_train = pd.concat([df_m_train, df_n_train])
    x_train = df_train[prop_columns].copy()
    y_train = df_train['y'].copy()

    index_test = df_candidate_pairs.index.difference(df_train.index)
    df_test = df_candidate_pairs.loc[index_test]
    df_test['y'] = 0
    index_test_match = index_test.intersection(df_matches.index)
    df_test.loc[index_test_match, 'y'] = 1
    x_test = df_test[prop_columns].copy()
    y_test = df_test['y'].copy()

    logging.info('splitting result: x_train=%s, y_train=%s, fn=%s, x_test=%s, y_test=%s',
        len(x_train), len(y_train), len_false_nonmatches, len(x_test), len(y_test))
    return x_train, y_train, x_test, y_test

def train_test_split(df_candidate_pairs, index_matches, train_size, columns_x, column_y='y', column_ml_phase=None):
    index_candidate_pairs = df_candidate_pairs.index
    index_matches_intersection = index_matches.intersection(index_candidate_pairs)
    len_matches_fn = len(index_matches.difference(index_candidate_pairs))
    logging.info('train_size=%s, candidate pairs (CP)=%s, matches in CP=%s, FN (outside CP)=%s, ',
        train_size, len(index_candidate_pairs), len(index_matches_intersection), len_matches_fn)

    df_candidate_pairs[column_y] = 0
    df_candidate_pairs.at[index_matches_intersection, column_y] = 1
    df_y = df_candidate_pairs[column_y]
    if column_ml_phase:
        df_candidate_pairs[column_ml_phase] = 'test'
    if train_size == 0:
        x_train = []
        x_test = df_candidate_pairs[columns_x].copy()
        y_train = []
        y_test =df_y
    else:
        df_cp = df_candidate_pairs[columns_x].copy()
        x_train, x_test, y_train, y_test = sklearn.model_selection.train_test_split(df_cp, df_y, train_size=train_size, shuffle=True, stratify=df_y)
        if column_ml_phase:
            df_candidate_pairs.at[x_train.index, column_ml_phase] = 'train'

    logging.info('split into x_train=%s, x_test=%s, y_train=%s, y_test=%s', len(x_train), len(x_test), len(y_train), len(y_test))

    return df_candidate_pairs, x_train, x_test, y_train, y_test

def split_df(df_train_test_split, df_scores, columns_x, column_y='y', column_ml_phase='ml_phase'):
    mask = (df_train_test_split[column_ml_phase] == 'train')
    df_train = df_train_test_split[mask].copy()
    df_test = df_train_test_split[~mask].copy()
    y_train = df_train[column_y]
    y_test = df_test[column_y]
    x_train = df_scores.loc[df_train.index][columns_x].copy()
    x_test = df_scores.loc[df_test.index][columns_x].copy()
    return x_train, x_test, y_train, y_test
