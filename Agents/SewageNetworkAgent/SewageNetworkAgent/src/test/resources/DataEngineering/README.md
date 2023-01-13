# **Primasens Sewage Data Instantiation**

This project provides functionality to instantiate data for the sewage network in Pirmasens. This repository provides tools to analyse the xml data structure and clean the provided data for further process; however, the data itself is confidential information and not available on github!

## Setting up a virtual environment

It is highly recommended to use a virtual environment for this project. The virtual environment can be created as follows (`python` command might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variables):

`(Windows)`

```
$ python -m venv ps_sewage_venv
$ ps_sewage_venv\Scripts\activate.bat
(ps_sewage_venv) $
```



The above commands will create and activate the virtual environment ps_sewage_venv in the current directory.

## Installation of required packages

Use the following command to install all required packages to run the project:

`pip install -r requirements.txt`

## Description of running the code

Available modules:

`create_HG_KG_instances.py` extracts data from XML file based on priority tags to and clean data for further processing. The output files created with this script are needed in the other two scripts.

`match_KG_main_sub_instances.py` uses the `main_datamodel.ttl` and `sub_datamodel.ttl` files to extract KG labels of sub and main networks and find the corresponding KG instance name of each label.

`match_HG_main_sub_instances.py` uses the `sub_network_connections_consolidated_wo_geodata.csv` file to extract HG labels of sub and main networks and then, find the corresponding HG instance name of each label.
Also, it does some data cleaning.

The `Data/raw/` directory needs to contain some initial data files for the code to take in 

The `Data/results/` directory contains all the output files generated during the running of scripts.
