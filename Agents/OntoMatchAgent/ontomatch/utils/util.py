import argparse
import collections
import json
import logging
import logging.config
import os
import pprint
import random
import sys
import yaml

import numpy as np
import pandas as pd

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

def read_config(path):
    with open(path) as json_config:
        config = json.load(json_config, object_pairs_hook=collections.OrderedDict)
    return config

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
        config = read_config(args.config)
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

    return config

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
