# **Sewage Data Preprocessing**

This project provides functionality to preprocess data required for instantiating a sewage network. It provides tools to analyze the XML data structure and clean the provided data for further processing.

## **Description of modules**

The `./Data/raw/` directory must contain all the required input files including `dictionary.csv`, `main_datamodel.ttl`, `sub_datamodel.ttl`, `priority_tags_in_ontology.csv`, `sub_network_connections_consolidated_wo_geodata.csv`, and `HALTUNGEN-ALLE-5-2022.xml`. Note that `main_datamodel.ttl`, `sub_datamodel.ttl`, and `sub_network_connections_consolidated_wo_geodata.csv` are created as output files by the scripts `instantiate_main_network.py`, `instantiate_sub_network.py`, and `assess_geospatial_sub_network.py` in a private git repository.

The `./Data/results/` directory contains all the output files generated during the running of scripts. It is worth noting that the number of columns inside the CSV files should not exceed 7500, otherwise this can cause index-out-of-bound errors elsewhere. Therefore, such files are split into several smaller ones.

Available scripts:

`create_HG_KG_instances.py` extracts data from XML file based on priority tags, and cleans data for further processing. The output files created with this script are needed in the other two scripts.

`match_KG_main_sub_instances.py` uses the `main_datamodel.ttl` and `sub_datamodel.ttl` files to extract KG labels of sub and main networks and find the corresponding KG instance name of each label.

`match_HG_main_sub_instances.py` uses the `sub_network_connections_consolidated_wo_geodata.csv` file to extract HG labels of sub and main networks and then, find the corresponding HG instance name of each label.
Also, it does some data cleaning.


## **Running the project**


### **Run with Docker**
The `Dockerfile` file contains the instructions to build an Image and run all the above scripts. To build and start the agent locally, open up the command prompt in the same directory as this README, run

`docker-compose up -d`

The `./Data` folder would be used as a Docker Volume and outputs will be generated in `./Data/results`.

### **Run with python virtual environment**

#### **Creating virtual environment**

The virtual environment can be created as follows (`python` command might need to be replaced with `py` depending on whether Python is specified in the system's `PATH` variables):

`(Windows)`

```
$ python -m venv ps_sewage_venv
$ ps_sewage_venv\Scripts\activate.bat
(ps_sewage_venv) $
```

The above commands will create and activate the virtual environment `ps_sewage_venv` in the current directory.

#### **Installation of required packages**

Use the following command to install all required packages to run the project:

`pip install -r requirements.txt`

#### **Running the scripts**

Run the scripts in the following order, using the following commands. The results will be generated in `./Data/results`.


```
(ps_sewage_venv) $ python create_HG_KG_instances.py 

(ps_sewage_venv) $ python match_KG_main_sub_instances.py

(ps_sewage_venv) $ python match_HG_main_sub_instances.py
```
