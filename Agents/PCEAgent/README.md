# Description #

This Python code contains a number of different models that can be trained to predict Power Conversion Efficiency (PCE) of organic photovoltaics (OPV). The models have been developed as part of the ["Predicting power conversion efficiency of organic photovoltaics: models and data analysis"](https://como.ceb.cam.ac.uk/preprints/268/) paper.

# Installation #

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Prerequisites ##
-----------------
- Anaconda installation (either Miniconda or Anaconda)

## How to install ##

Provided all above prerequisites are met the package can be installed via the following steps:

(Windows)
1. Open Anaconda Command Prompt
2. Navigate to the project directory
3. Run:
```console
    install_script.sh [options]

    options:
      -v              : creates conda virtual environment for this project in the project directory and
                        installs all its dependencies. Note, if the same named environment already exists
                        it will be removed and created again
      -n VENV_NAME    : name of the virtual environment to be created, if not provided
                        default `oscml_venv` will be used instead
      -g              : install cuda-enabled version allowing to run training on gpu
      -i              : installs the project in the currently active virtual environment
      -e              : enables developer mode installation
      -r              : downloads project resources from the remote location
      -h              : prints this help message

    example usage:
    ./install_script.sh -v -i -e -r      this will create default conda virt. env. `oscml_venv`,
                                         install the project with all its dependencies in a developer
                                         mode and pull all its resources from the remote server
```

(Linux)
1. Add Miniconda to your PATH in .bashrc by running `conda init` command
2. On some linux systems you may need to first run:
```console
    module load miniconda/3/
```
2. After that, acticate the base conda environment via:
```console
    conda activate
```
3. Navigate to the project directory
4. Run:
```console
    install_script.sh [options]
```

The steps above should create a separate conda virtual environment in a default location used by conda to store environments, install all required packages and download the project data from the remote server. The optional `-n VENV` flag can be used to set the virtual environment name and if not provided the name will defaut to `oscml_venv`. `PLEASE NOTE`, if indicated conda environment name already exists, it will be `REMOVED` and created again.

After successful installation, please do not forget to activate the newly created conda environment to run the code via the following command:
```console
    conda activate VENV_NAME
```

# How to use #

After successful installation, the package can be used via the following command:
```console
    oscml <configFile> [options]

    options:
        --hpo=DOHPO                               Run hyperparameter optimisation flag (default True)
        --contour_plot=DOCONTPLOT                 Make contour plot flag (default False)
        --best_trial_retraining=DOBESTRETR        Run the best trial retraining flag (default True)
        --transfer_learning=DOTRANSLEARN          Run the transfer learning flag (default False)
        --predict=DOMODELPREDICTION               Run the model predict flag (default False)
        --trials=TRIALS                           No. of hpo trials to run (default 10)
        --predict_input=SMILESLIST                List of smiles strings to be passed to saved model for pce prediction (no default)
        --actual_output=PCELIST                   List of pce values associated with defined SMILESLIST (no default)
        --jobs=JOBS                               No. of parallel hpo jobs (default 1)
        --epochs=EPOCHS                           No. of epochs to use during training (if applicable, no default)
        --batch_size=BATCHSIZE                    Batch size to use during training (if applicable, no default)
        --patience=PATIENCE                       Early stopping patience parameter to use during training (if applicable, no default)
        --min_delta=MINDELTA                      Early stopping min delta parameter to use during training (if applicable, no default)
        --cross_validation=CROSSVAL               No. of inner cross validation folds (default 0)
        --nested_cross_validation=NESTCROSSVAL    No. of outer cross validation folds (default 0)
        --study_name=STUDYNAME                    Name of the study (default 'oscml_study')
        --storage=STORAGE                         Sqlite storage for this run (default 'sqlite:///oscml_study.db')
        --timeout=TIMEOUT                         Stop study after the given number of second(s). If this argument is not set,
                                                  the study is executed without time limitation (default None)
        --storage_timeout=STORTIMEOUT             Maximum time in seconds for the storage access operations (default 1000)
        --load_if_exists=LOADIFEXISTS             In case where a study already exists in the storage load it (default True)
        --log_config_file=LOGCONFIG               Logging configuration file (default configuration)
        --log_main_dir=LOGDIR                     Main logging directory (default './logs')
        --log_sub_dir_prefix=LOGSUBDIRPREF        Log subfolder prefix (default 'job_')
        --log_file_name=LOGBASENAME               Log file base name (default 'oscml.log')
        --seed=SEED                               Random seed to use (default 1)
```

where the `<config_file_name>` is the name of the config file that specifies the job that should be run and `OPTIONS` indicate any additional command line options that can be used to control the job (if any). `Note` that all additional command line options can be also defined in the config file. More information about the config file syntax and command line options can be found in the code and in the sample config files provided as part of this repository.


# Testing #

All the unit tests for this project are collated in the `tests` folder and can be run via the following command:
```console
    python .\tests\other_tests.py
```

All the regression tests for this project are collated in the `tests` folder and can be run via the following command:
```console
    python .\tests\regression_tests.py
```

`Note` that the regression tests are only for developers and the results are platform dependent. Therefore, it is not guaranteed that the tests will pass on different hardware.

# Important note for the VS CODE users #

It has been found that the VS CODE and Anaconda integration is not always smooth. One problem that users very often experience is VS CODE's failure to fully load Anaconda's virtual environment, which in turn leads to random package import failures (e.g. numpy). A workaround that seems to fix this issue is to always launch the VS CODE from a command line which has the conda environment already activated. As an example, launching the VS CODE on Windows should be done as follows:

1. Open Anaconda Command Prompt
2. Navigate to the project directory
3. Activate the "osc_env" environment
4. Launch the VS CODE via the following command:
```console
    code .
```

# Authors #

Andreas Eibeck, Daniel Nurkowski, Angiras Menon, Jiaru Bai

## Useful links ##

* [Preprint 268](https://como.ceb.cam.ac.uk/preprints/268/)
