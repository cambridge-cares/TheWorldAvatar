import logging
import logging.config
import os
import pprint
import yaml

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


def init_logging(src_directory, dst_directory):

    # file and console logging with Python's standard logging library
    log_config_file = src_directory + '/conf/logging.yaml'
    log_dir = dst_directory + '/logs'
    name = 'ontomatch'
    init_file_logging(log_config_file, log_dir + '/' + name + '.log')

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
