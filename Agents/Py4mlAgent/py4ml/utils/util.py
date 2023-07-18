import logging
import logging.config
import os
import pprint
import yaml
import sklearn.metrics
import pkg_resources
import datetime

_RES_DIR = pkg_resources.resource_filename(__name__,os.path.join('..','resources'))
_RES_LOG_CONFIG_FILE = os.path.join(_RES_DIR,'logging.yaml')

def init_file_logging(log_config_file='default', log_main_dir='default', \
                      log_sub_dir_prefix='default', log_file_name='default', use_date_time=True):
    if log_config_file=='default':
        log_config_file = _RES_LOG_CONFIG_FILE
    if log_main_dir=='default':
        log_main_dir = './logs/'
    if log_sub_dir_prefix=='default':
        log_sub_dir_prefix = 'hpo_'
    if log_file_name=='default':
        log_file_name = 'py4ml.log'

    current_date_time = ''
    if use_date_time:
        current_date_time = datetime.datetime.now().strftime('%Y%m%d_%H%M%S')

    log_file = os.path.normpath(os.path.join(log_main_dir, \
                log_sub_dir_prefix + current_date_time,log_file_name))


    print('initializing logging with log config file=', log_config_file, ', log file=', log_file)
    with open(log_config_file, 'r') as f:
        # always use safe_load to avoid reading and executing as YAML serialized Python code
        # yaml returns a dictionary
        log_cfg = yaml.safe_load(f.read())

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
    return log_file

def concat(*args):
    if len(args) == 1:
        return args[0]
    else:
        message = ''
        for m in args:
            message += str(m) + ' '
        return message

def calculate_metrics(y_true_np, y_pred_np):
    mae = sklearn.metrics.mean_absolute_error(y_true_np, y_pred_np)
    # mse is the mean squared error because squared=True by default
    mse = sklearn.metrics.mean_squared_error(y_true_np, y_pred_np, squared=True)
    rmse = sklearn.metrics.mean_squared_error(y_true_np, y_pred_np, squared=False)
    R2 = sklearn.metrics.r2_score(y_true_np, y_pred_np)

    return {'mse':mse, 'rmse': rmse, 'R2':R2, 'mae': mae, 'count': len(y_true_np)}
