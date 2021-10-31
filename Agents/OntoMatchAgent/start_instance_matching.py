import argparse
import collections
import json
import logging
import os
import random
import time

import numpy as np

import coordinator
import util

def start(config_dev=None):

    print('current working directory=', os.getcwd())

    parser = argparse.ArgumentParser()
    parser.add_argument('--config', type=str, default=None)
    args = parser.parse_args()

    if args.config:
        with open(args.config) as json_config:
            config = json.load(json_config, object_pairs_hook=collections.OrderedDict)
    else:
        config = config_dev

    seed = config['numerical_settings'].get('seed')
    np.random.seed(seed)
    random.seed(seed)

    util.init_logging('.', '..')
    logging.info('current working directory=%s', os.getcwd())
    logging.info('args=%s', args)
    logging.info('config=%s', config)

    starttime = time.time()
    agent = coordinator.Agent()
    result = agent.start(config)
    timenow = time.time()-starttime
    logging.info('elapsed time in seconds=%s', timenow)
    return result


if __name__ == '__main__':
    start()
