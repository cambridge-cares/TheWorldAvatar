# Description #

This repository contains a generic python code base for training and optimising a number of different ML or neural models. By default, the code provides Multilayer Perceptron neural model, Random Forests and Support Vector Machines regressors. Hyperparameter optimisation is driven by the [optuna](https://optuna.org/) package. The code is input file driven, where the following things can be specified:
- model type
- model hyperparameters (either fixed or ranges plus the sampling technique)
- data source (x features, y output)
- goals - these define what actions will be taken, choose from:
    - `hpo` (hyper-parameter optimisation, if no hpo sampling options defined, e.g. all model parameters are fixed, this is equivalent to model training)
    - `best_trial_retraining` (hpo can be followed by the best trial retraining, this step trains the best model again, but this time on combined training and validation datasets)
    - `predict` (use trained models in production)
    - `contour_plot` (only makes sense with the hpo goal and a rather large number of trials)
    - `transfer_learning` (currently not supported by any of the default models)

Multiple goals can be defined, e.g. it is very common to define `hpo` and `best_trial_retraining` goals.

# Installation #

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Prerequisites ##
-----------------
- Anaconda installation (either Miniconda or Anaconda)

## Docker ##

Simply use included docker-compose.yml file to install the app and all its dependencies. Once the container is spined, copy required data and log into the container to run your models.

## How to install locally ##

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
                        default `py4ml_venv` will be used instead
      -g              : install cuda-enabled version allowing to run training on gpu
      -i              : installs the project in the currently active virtual environment
      -e              : enables developer mode installation
      -h              : prints this help message

    example usage:
    ./install_script.sh -v -i -e    this will create default conda virt. env. `py4ml_venv`,
                                    install the project with all its dependencies in a developer mode

```

(Linux)
1. Add Miniconda to your PATH in .bashrc by running `conda init` command
2. On some linux systems you may need to first run:
```console
    module load miniconda/3/
```
2. After that, activate the base conda environment via:
```console
    conda activate
```
3. Navigate to the project directory
4. Run:
```console
    install_script.sh [options]
```

The steps above should create a separate conda virtual environment in a default location used by conda to store environments, install all required packages and download the project data from the remote server. The optional `-n VENV` flag can be used to set the virtual environment name and if not provided the name will defaut to `py4ml_venv`. `PLEASE NOTE`, if indicated conda environment name already exists, it will be `REMOVED` and created again.

After successful installation, please do not forget to activate the newly created conda environment to run the code via the following command:
```console
    conda activate VENV_NAME
```

# How to use #

## CLI

After successful installation, the package can be used via the following command:
```console
    py4ml <configFile> [options]

    options:
        --hpo=DOHPO                               Run hyperparameter optimisation flag (default True)
        --contour_plot=DOCONTPLOT                 Make contour plot flag (default False)
        --best_trial_retraining=DOBESTRETR        Run the best trial retraining flag (default True)
        --transfer_learning=DOTRANSLEARN          Run the transfer learning flag (default False)
        --predict=DOMODELPREDICTION               Run the model predict flag (default False)
        --trials=TRIALS                           No. of hpo trials to run (default 10)
        --predict_input=INPUTS                    List of inputs to be passed to saved model for pce prediction (no default)
        --actual_output=OUTPUTS                   List of outputs associated with defined INPUTS (no default)
        --jobs=JOBS                               No. of parallel hpo jobs (default 1)
        --epochs=EPOCHS                           No. of epochs to use during training (if applicable, no default)
        --batch_size=BATCHSIZE                    Batch size to use during training (if applicable, no default)
        --patience=PATIENCE                       Early stopping patience parameter to use during training (if applicable, no default)
        --min_delta=MINDELTA                      Early stopping min delta parameter to use during training (if applicable, no default)
        --cross_validation=CROSSVAL               No. of inner cross validation folds (default 0)
        --nested_cross_validation=NESTCROSSVAL    No. of outer cross validation folds (default 0)
        --study_name=STUDYNAME                    Name of the study (default 'py4ml_study')
        --storage=STORAGE                         Sqlite storage for this run (default 'sqlite:///py4ml_study.db')
        --timeout=TIMEOUT                         Stop study after the given number of second(s). If this argument is not set,
                                                  the study is executed without time limitation (default None)
        --storage_timeout=STORTIMEOUT             Maximum time in seconds for the storage access operations (default 1000)
        --load_if_exists=LOADIFEXISTS             In case where a study already exists in the storage load it (default True)
        --log_config_file=LOGCONFIG               Logging configuration file (default configuration)
        --log_main_dir=LOGDIR                     Main logging directory (default './logs')
        --log_sub_dir_prefix=LOGSUBDIRPREF        Log subfolder prefix (default 'job_')
        --log_file_name=LOGBASENAME               Log file base name (default 'py4ml.log')
        --seed=SEED                               Random seed to use (default 1)
```

where the `<config_file_name>` is the name of the config file that specifies the job that should be run and `OPTIONS` indicate any additional command line options that can be used to control the job (if any). Note that all the listed CLI options can (and should) be defined in the config file in most cases. The CLI options, if defined, take precedence over the config file options, thus can be used to quickly change something in the pre-defined job.

## Config file syntax

ML and neural models simulations are controlled via the JSON config file. Below, a detailed information on the file syntax and options is given:


### Simulation goals

The `goals` field controls the simulation behaviour by specifying which pre-defined goal(s) to run. There are five goals to choose from where more than one goal can be selected. As an example, it is quite common to run the `hpo`, `contour_plot` and `best_trial_retraining` goals one after another. The order of goals execution is as listed in the below snippet:

```json
{
    "goals": {
        "hpo": false,
        "contour_plot": false,
        "best_trial_retraining": false,
        "transfer_learning": false,
        "predict": false
    },
}
```

The `hpo` goal can be further configured to either run a model hyper-parameter optimisation (n_trials>0) or just the model training (n_trials=0). If enabled to run model hyper-parameter optimisation, by setting the number of trials to be greater than zero in the `training` section (covered below), it will perform automatic search for the most optimal model parameters. Note that only parameters indicated via a special syntax in the `model_specific` input file section will be included in the optimisiation.


The `contour_plot` option can be used to create contour plots showing the loss function vs sampled parameter space. This option requires the `hpo` option enabled with at least 10 trials.

The `best_trial_retraining` option should be used after the hyper-parameter optimisation procedure. Its purpose is to train the best performing model again on a slightly larger set of data by combining the training and validation sets (no longer required for hpo). Note that in case of the neural models using the early stopping callback as a regularization method, the validation set is still required. In that case, however, the training and hpo validation sets will be combined, reshuffled and split again.

The `transfer_learning` option can only be used by the neural models. It allows to initialise the random weights of the current, untrained model using the previously trained model. This allows to speed up and improve the model training on a new set of data. Note that both models should have the same architecture (the same number of layers, neurons).

The `predict` option is used to make predictions after training the model. The option would read the trained model checkpoint file and run it against provided inputs.

### Simulation numerical settings

These settings control the simulation numerical behaviour. The default (and recommended) settings are shown below. They ensure predictable and consistent results provided the simulations are run on the same hardware.

```json
{
    "numerical_settings":{
        "seed": 1,
        "cudnn_deterministic": true,
        "cudnn_benchmark": false
    },
}
```


### Dataset settings

These settings specify which data will be used for model hyper-optimisation and / or training and how data will be pre-processed and split. Note that only csv data format is supported. All the possible options are shown below:

```json
{
    "dataset": {
        "src": "./some_data.csv",
        "transform_type": "z-transform",
        "x_column": [
            "feature 1",
            "feature 2",
            "feature 3"
        ],
        "y_column": [
            "y(x)"
        ],
        "split": [ 0.7, 0.15, 0.15 ]
    },
}
```

The `src` option is used to provide the data file source path.

The `transform_type` is used to specify how to transform the data. Note that currently the only available options are the `z-transform` or no transform at all (none). If the `z-transform` option is chosen, the training, validation and test datasets x and y columns will be column-wise z-transformed based on the means and standard deviations calculated on the training set. The model is then trained and all the scores (mse, rmse, mae, r, R2) are calculated with respect to that transformed data. Note that the data transformation details are saved together with the model in a checkpoint file. This simplifies using the model for predictions as no prior knowledge on how to prepare data before passing it to the model is required as this is read from the checkpoint file as well.

The `x_column` is used to specify which dataset columns should be included as features.

The `y_column` is used to specify which dataset column should be included as an output.

The `split` option controls how the dataset is split into the training, validation and test sets respectively. The dataset can be either split at runtime, by providing the size ratio of each sub-dataset, e.g. `"split": [0.7, 0.15, 0.15]` would correspond to the training set containing 70\% of data and validation and test sets containing 15\% each. An alternative option is to include the split in the dataset itself by introducing an extra "ml_phase" (or "ml_phase_fold_n" in case of nested cross validation) column. As an example, the following data set:

```csv
x1  ,  x2,  x3,   y,ml_phase
... ,... , ..., ..., train
... ,... , ..., ..., val
... ,... , ..., ..., test
...
```

would place the first, second and third row into the training, validation and test sets respectively.

In case of the nested cross-validation, predefining the split for each outer fold in the dataset is the only supported option at the moment. As an example, the dataset for a 3-fold nested cross-validation should look like this:

```csv
x1  ,  x2,  x3,   y,ml_phase_fold_0,ml_phase_fold_1,ml_phase_fold_2
... ,... , ..., ...,          train,          train,          test
... ,... , ..., ...,            val,            val,         train
... ,... , ..., ...,           test,            val,         train
...
```


# Authors #

Andreas Eibeck, Daniel Nurkowski, Angiras Menon, Jiaru Bai

## Useful links ##

* [Preprint 268](https://como.ceb.cam.ac.uk/preprints/268/)
