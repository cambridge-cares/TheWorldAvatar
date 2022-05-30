# Description #

This repository contains a generic python code base for training and optimising a number of different ML or neural models. By default, the code provides Multilayer Perceptron neural model, Random Forests and Support Vector Machines regressors. Hyperparameter optimisation is driven by the [optuna](https://optuna.org/) package. The code is input file driven, where the following things can be specified:
- model type
- model hyperparameters (either fixed or ranges plus the sampling technique)
- data source (x features, y output)
- goals - these define what actions will be taken, choose from:
    - `hpo` (hyper-parameter optimisation, if no hpo sampling options defined, e.g. all model parameters are fixed, this is equivalent to model training)
    - `best_trial_retraining` (hpo can be followed by the best trial retraining, this step trains the best model again, but this time on combined training and validation datasets)
    - `predict` (use trained models to make predictions)
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

# Config file syntax

ML and neural models simulations are controlled via the JSON config file. Below, a detailed information on the file syntax and options is given:


## Simulation goals

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

## Simulation numerical settings

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


## Dataset settings

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

The `transform_type` is used to specify how to transform the data. Note that currently the only available options are the `z-transform` or no transform at all (none). If the `z-transform` option is chosen, the training, validation and test datasets x and y columns will be column-wise z-transformed based on the means and standard deviations calculated on the training set. The model is then trained and all the scores (mse, rmse, mae, r, R2) are calculated with respect to that transformed data. Note that the data transformation details are saved together with the model in a checkpoint file. This simplifies using the model for predictions as no prior knowledge on how to prepare data before passing it to the model is required as this is read from the checkpoint file and automatically applied to the data.

The `x_column` is used to specify which dataset columns should be included as features.

The `y_column` is used to specify which dataset column(s) should be included as an output.

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

Note that the Random Forests regressor and Multilayer Perceptron support multiple target variables (y). In such case, simply include extra columns in the dataset and list them in the `y_column` parameter. Support Vector regressor does support multiple columns. In general, training a model for multiple targets is much harder than if there is only one target. A better strategy could be to train separate models for each target.

## Model settings

The model settings allow to specify a model and set its hyper-parameters to use in the simulation. The example model section in the json file is presented below:

```json
{
    "model": {
        "name": "Model_Name",
        "model_specific": {
            // model specific parameters
        }
    }
}
```

The `name` option specifies which model to use in the simulation. This is a required field. As mentioned previously, the following models are supported:
- Multilayer perceptron `"name": MLP`,
- Random Forests regressor `"name": RF`,
- Support Vector Machines regressor `"name": SVR`,

The `model_specific` section contains all model specific hyper-parameter settings. In case of the `MLP` these are:
- `mlp_layers`   - number of hidden layers
- `mlp_units`    - number of neurons in each layer
- `mlp_dropouts` - dropout fraction to use in each layer

Note that the number of neurons in the input and output layers is automatically set based on the features and target data size.

In case of Random Forests and Support Vector Machines regressors these are any parameters listed in the `sklearn` documentation for each model constructor (see [RF](https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestRegressor.html) and [SVR](https://scikit-learn.org/stable/modules/generated/sklearn.svm.SVR.html)).

Do note that each supported model has its own module defining how the parameters are created and pre-processed before passing them to the model constructor. If any modifications are required, simply look into the `py4ml.hpo.hpo_<model_name>.py` module and look into the `model_create` function. The default behaviour is to pass the parameters as they are defined in the input file.


Defining model specific hyper-parameters offers some flexibility. The syntax follows the optuna syntax for setting hyper-parameters with a couple of modifications. The list below shows some example options, whereas for a more detailed overview please visit the [optuna documentation](https://optuna.readthedocs.io/en/stable/reference/distributions.html):

```json
{
"model_specific":{
    // 1.  Fixed value, the parameter wont change during hpo.
	"param_1": 1,

    // 2.  Fixed list of values, the parameter wont change during hpo.
	"param_2": [1,3,4],

    // 3.  Fixed list of values, of a desired length, the last item on the
    //     values list will be used to pad any missing values to satisfy the
    //     length criteria, the parameter wont change during hpo.
	"param_3": {"values":[1,3,4],"length":100},

    // 4.  Similar to option 3, but this time the length is set based
    //     on the length of another parameter. The parameter that the length is
    //     based on must be defined prior to the current one
    //     the parameter wont change during hpo.
	"param_4": {"values":[1,3,4],"length":"param_3"},

    // 5.  Similar to option 4, but this time a value of the previous parameter is
    //     used to set the length of the current parameter
    //     the parameter wont change during hpo.
	"param_5": {"values":[1,3,4],"length":"param_3[2]"},

    // 6.  Parameter value will be randomly sampled between 1 and 12 (inclusive) during hpo.
	"param_6": {"type":"int","low":1,"high":12},

    // 7.  This defines parameter_7 as a list of length 6, where each item is randomly
    //     sampled between 5 and 10 (inclusive) during hpo.
    "param_7": {"type":"int","low":5,"high":10, "length":6},

    // 8.  Similar to option 7, except that the parameter length is set based on
    //     the length of another parameter.
    "param_8": {"type":"int","low":5,"high":10, "length":"param_5"},

    // 9.  Similar to option 8, except that the parameter length is set based on
    //     the value of another parameter.
    "param_9": {"type":"int","low":5,"high":10, "length":"param_7[3]"},

    // 10. Similar to option 9, except the first item on the parameter list is not
    //     sampled, but set to another parameter value for all the hpo trials,
    //     all other items are randomly sampled between 1 and 4
	"param_10": {"type":"int","low":1,"high":4,"starting_value":"param_1", "length":6},

    // 11. Similar to option 10, except the first three items on the parameter list
    //     are not sampled, but set to fixed values for all the hpo trials, all other
    //     items are randomly sampled between 1 and 4.
	"param_11": {"type":"int","low":1,"high":4,"starting_value":[1,2,3], "length":6},

    // 12. In each trial, the parameter value is defined as a list of length 4, where
    //     each item is randomly sampled between 1 and 10 (inclusive) in a decreasing
    //     direction. This means that each item on the list will be smaller or equal
    //     to the preceding item. The use of the "direction" option is
    //     usually NOT RECOMMENDED as it quite often better to set a larger number of
    //     trials and rely on the optimiser to do its job.
	"param_12": {"type":"int","low":1,"high":10,"length":4,"direction":"decreasing"},

    // 13. Similar to option 12, except the first three items are not sampled, but
    //     directly set to fixed values for all hpo trials. All other items are sampled
    //     in a decreasing direction with respect to the last fixed item (8).
    "param_13": {"type":"int","low":1,"high":10,"length":5,"starting_value":[10,8,8],"direction":"decreasing"},

    // 14. Similar to option 12, but the values are sampled in the increasing order.
    "param_14": {"type":"int","low":1,"high":10,"length":4,"direction":"increasing"},

    // 15. Similar to option 12, but the values are sampled in the constant order.
    //     This means that there is only one sampling per list, and all items have
    //     the same random value.
    "param_15": {"type":"int","low":1,"high":10,"length":4,"direction":"increasing"},

    // 16. The paremeter values will be randomly sampled from a predefined choices list.
    "param_16": {"type":"categorical","choices": [0.5, 0.1, 2.0, 3.0]},
    }
}
```


## Training settings

The training settings control specifics of the model training and hyper-parameter optimisation process. The possible options are defined below:

```json
{
    // OPTIONS SPECIFIC TO TORCH NEURAL NETWORK MODELS ONLY
    // ---------------------------------------------------------------
    // These are torch optimiser settings, only relevant for the
    // torch neural models. See https://pytorch.org/docs/stable/optim.html
    "optimiser": {
        "name": "Optimiser_Name",
        // Optimiser_specific parameters
        // The parameters can be randomly sampled during the
        // hpo procedure. Simply follow the hpo parameters syntax defined
        // in the previous section.
    },
    "batch_size": 20,
    // number of epochs to train the model for
    "epochs": 100,
    // These are early stopping parameters. See torch documentation
    // for further details
    // note that setting patience to zero, disables early stopping
    // regularization
    "es_patience": 80,
    "es_min_delta": 0.0,
    "es_mode": "min",
    // choose from supported metrics that are defined below
    // note that some metrics will require different es_mode,
    // if not defined, defaults to the "mse"
    "es_monitor": "mse",
    // Checkpoint callback settings
    "chp_monitor": "mse",
    "chp_mode": "min",
    //
    // COMMON HPO OPTIONS
    // ---------------------------------------------------------------
    // optimisation direction, see optuna documentation
    "direction": "minimize",
    // metric used for the model hyper-parameter optimisation
    // choose from: "mse", "rmse", "mae" and "R2"
    "metric": "mse",
    // number of hpo trials
    "trials": 100,
    // name of the study, will be used to create study database
    // in the defined storage, and also to read the study back
    // from it for hpo continuation
    "study_name": "study_name",
    // if present, will create sqlite database file and store all hpo
    // details, this allows to restart the hpo study to run more trials
    "storage": "sqlite:///<study_path>/<study_database_filename>.db",
    // this affects how long optuna would wait for accessing the storage
    // in the study database in case of an increased traffic.
    "storage_timeout": 1000,
    // this controls whether or not to load the previous study from the
    // storage in order to continue the hpo
    "load_if_exists": true,
    // how many hpo trials to run in parallel. note that setting it to a high
    // number would affect the course of hpo where usually more trials will be
    // required to optimise a study compared to a non-parallel case.
    // as an example, running 40 trials, one after another ("jobs": 1), would
    // result in every past trial informing the parameters for the next one
    // running 40 trials, where 10 are run in parallel ("jobs": 10) would lead
    // to only a fraction of past trials informing the next trial as not all
    // would finish at the same time. Increasing the number of trials for a
    // parallel case usually is one possible option to counter this problem.
    "jobs": 1,
    // use cross-validation with n-number of folds (if 0 or false)
    // no cross-validation is performed
    "cross_validation": 0,
    // use nested cross-validation with n-number of outer folds (if 0 or false)
    // no nested cross-validation is performed
    // it is usually easier to perform nested cross validation manually
    // as n separate runs rather than via this setting as then it is easier
    // to restart each outer fold if something goes wrong.
    // use this option for rather quick studies that do not require restart
    // capabilities
    "nested_cross_validation": 0
}
```

## Predict settings

The predict settings are used to control how and which model is used to make predictions on new data. The json snippet below shows all the possible settings:
```json
{
    "predict_settings": {
        "ckpt_path": "./path/trial_*/best_trial_retrain_model*.ckpt",
        "predict_input": [
            [ 0.1, 0.2, 0.3 ],
            [ 0.4, 0.6, 0.7 ],
            [ 0.3, 0.1, 0.9 ]
        ],
        "actual_output": [
            0.7,
            0.8,
            0.9
            ]
    }
}
```

The `ckpt_path` parameter defines path to the model checkpoint file. The path supports globbing character `*` on model name and trial subdirector (if present). If globbing is present, it will use the first found match (so use with caution).

The `predict_input` is used to set data (x features) for model prediction. The same type of data should be used here as the one used for the model training. If the `z-transform` was used during the model training, the predict input data will be automatically transformed.

The `actual_output` is an optional field that allows to pass the actual target values (y column(s)) corresponding to the predict inputs. If the `z-transform` was used during the model training, the predicted outputs will be automatically transformed back for a direct comparison. The example above is for a case with only a single y column. In case of multiple y columns, the following syntax should be used:

```json
{
    "actual_output": [
        [ 0.7, 0.4, ... ],
        [ 0.8, 0.1, ... ],
        [ 0.9, 0.3, ... ]
    ]
}
```


## Post-processing settings

These settings control certain aspects of the simulation goals defined above. The following options are supported:

```json
{
    "post_processing":{
        // this is used only if the contour plot goal is enabled
        // and allows to change the default directory where the plot
        // will be saved
        "contour_plot_alt_dir": "dir",
        // this is used only if the best trial retraining goal is
        // enabled and allows to create an extra regression plot
        "regression_plot": false,
    },
}
```

## Logging settings

The logging settings allow to control the location and naming of the simulation logs. All the possible options are given below:

```json
{
    "logging_settings": {
        "log_main_dir": "./logs",
        "log_sub_dir_prefix": "job_",
        "log_file_name": "py4ml.log",
        "use_date_time": true,
        "log_config_file": "logging.yaml"
    },
}
```

The `log_main_dir` defines the main direction where all the simulation logs will be stored. Defaults to `./logs` if not given. If directory does not exists it will be created.

The `log_sub_dir_prefix` defines the sub-directory in the main directory where all the simulation logs will be stored. Defaults to `job_` if not defined. If directory does not exists it will be created.

The `use_date_time` controls whether or not to include the current date and time in the log subdirectory name. Defaults to true if not provided.

The `log_file_name` parameter defines the log file base name. Defaults to the `py4ml.log` if not defined.

The `log_config_file` defines location of the logging configuration file that allows to customise the application logging behaviour.


# Note to developers

The `py4ml` code is provided as is, without any guarantee that it will work in all cases. The code still requires major refactoring and cleanup, however, it is believed it might be useful even in its current state. For any further development it is strongly recommended to choose a machine on which all the regression tests will be run and uncomment the `#pd.testing.assert_frame_equal(refData[key],testData[key], rtol=0.05, atol=1e-5)` line in the `test_models.py` file, which would enable numerical results checking.

The code has been tested on fake data as part of included automated tests as well as on open source [aquatic toxicity data](https://archive.ics.uci.edu/ml/datasets/QSAR+aquatic+toxicity). The provided examples in the `conf` directory contain input files for training models on the `aquatic toxicity data`. Simply download the data from the provided link, replace `;` field separator with `,` and add the column headers and put such processed data file into `data\processed` directory. The provided examples, hopefully, should work out of the box.

The state of the `py4ml` code is not ideal. There is quite a lot of duplication, shortcuts and bad coding practices. This is due to a very short time scale of the `PCE` project during which the code was developed and due to ever changing requirements. Besides, producing a `generic` ML code base was not the main goal of the `PCE` project. Nevertheless, some effort has been undertaken to extract the generic bits out of the `PCE` code. This is how the `py4ml` materialised. It is believed that another round of proper refactoring is required to bring this code to a maintainable state.

In order to add another model to `py4ml` please follow the following steps:
- if the model creation is somewhat complicated and requires some customisation, add the `py4ml.models.model_<model_name>.py` model that would contain model construction specifics (e.g. mlp model),
- add `py4ml.hpo.hpo_<model_name>.py` module that would specify all the objectives / goals that the new model supports,
- add the model objective call to the `JobHandler._initObjective` method,
- add tests




# Authors #

Andreas Eibeck, Daniel Nurkowski, Angiras Menon, Jiaru Bai

## Useful links ##

* [Preprint 268](https://como.ceb.cam.ac.uk/preprints/268/)
