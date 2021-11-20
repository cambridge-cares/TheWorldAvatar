# Description #

This Python code contains a number of different models that can be trained to predict Power Conversion Efficiency (PCE) of organic photovoltaics (OPV). The models have been developed as part of the ["Predicting power conversion efficiency of organic photovoltaics: models and data analysis"](https://como.ceb.cam.ac.uk/preprints/268/) paper.

# Installation #

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Prerequisites ##
-----------------
- Anaconda installation (either Miniconda or Anaconda)
- Access to the internet

## How to install ##

Provided all above prerequisites are met the package can be installed via the following steps:

(Windows)
1. Open Anaconda Command Prompt
2. Navigate to the project directory
3. Run:
```console
    install_script.sh -a
```

(Linux)
1. Add Miniconda to your PATH in .bashrc by running "conda init" command
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
    install_script.sh -a
```

The steps above should create a separate conda virtual environment in a default location used by conda to store environments, install all required packages and download the project data from the remote server. During the installation, the script will prompt for the name of the conda virtual environment, `VENV_NAME`, to be used for the project. `PLEASE NOTE`, if indicated conda environment name already exists, it will be `REMOVED` and created again.

After successful installation, please do not forget to activate the newly created conda environment to run the code via the following command:
```console
    conda activate VENV_NAME
```

# How to use #

After successful installation, the package can be used via the following command:
```console
    oscml_run --config CONFIG_FILE_NAME OPTIONS

    OPTIONS:
    --trials          default= 1        - number of hpo trials to use
    --jobs            default= 1        - number of hpo trials to be run in parallel
    --storage         default= None     - name and type of the db storage (only sqlite type supported at the moment)
    --study_name      default= None     - hpo study name
    --load_if_exists  default= False    - if study exists in the defined storage, load it and continue hpo run
    --timeout         default= None     - timeout on accessing the hpo storage
```

where the `CONFIG_FILE_NAME` is the name of the config file that specifies the job that should be run and `OPTIONS` indicate any additional command line options that can be used to control the job (if any). `Note` that all additional command line options can be also defined in the config file and if that is the case, the config file options take precedence over the command line options. More information about the config file syntax and command line options can be found in the code and in the sample config files provided as part of this repository.


# Testing #

All the unit tests for this project are collated in the `tests` folder and can be run via the following command:
```console
    python .\tests\run_oscml_tests.py
```
`Note` that the tests might take a while to complete.

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
