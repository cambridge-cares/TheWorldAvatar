import pkg_resources
import os

_RES_DIR = pkg_resources.resource_filename(__name__,os.path.join('..','resources'))
_RES_LOG_CONFIG_FILE = os.path.join(_RES_DIR,'logging.yaml')

DEFAULTS_ = {
    "goals":{
        "hpo": False,
        "contour_plot": False,
        "best_trial_retraining": False,
        "transfer_learning": False,
        "predict": False
    },
    "numerical_settings":{
        "seed": 1,
        "cudnn_deterministic": True,
        "cudnn_benchmark": False
    },
    "training":{
        "metric": "mse",
        "cross_validation": 0,
        "nested_cross_validation": 0,
        "es_mode": "min",
        "es_monitor": "mse",
        "chp_mode": "min",
        "chp_monitor": "mse",
        "direction": "minimize",
        "trials": 10,
        "jobs": 1,
        "timeout": None,
        "study_name": "py4ml_study",
        "storage": "sqlite:///py4ml_study.db",
        "storage_timeout": 1000,
        "load_if_exists": True,
    },
    "logging_settings":{
        "log_main_dir": "./logs",
        "log_sub_dir_prefix": "job_",
        "log_config_file": _RES_LOG_CONFIG_FILE,
        "log_file_name": "py4ml.log",
        "use_date_time": True
    },
    "post_processing":{
        "contour_plot_alt_dir": None,
        "regression_plot": False
    },
    "transfer_learning": {
        "freeze_and_train": False,
    },
    "predict_settings": {
        "predict_input": [],
        "actual_output": []
    },
}