# Data screening

This sub-package provides a set of scripts to analyse the capabilities of the HM Land Registry's Open Data SPARQL endpoint. It is highly recommended to create a virtual environment to run the scripts in this sub-package.

## How to create a Python virtual environment and install required packages (in Windows):

1) Open `cmd` terminal and navigate into project repository

2) Create virtual environment `name` using venv (`python` might need to be replaced with `py` depending on whether Python is specified in system's `PATH` variable):
```
py -m venv <name>
```

3) Activate virtual environment by:
```
<name>\Scripts\activate
```

4) Install requirements listed in `requirements.txt`:
```
py -m pip install --upgrade pip
py -m pip install -r requirements.txt
```

## Provided scripts

All provided scripts are intended to be run as main scripts.

> The `land_registry_sparql_to_rdf.py` and `land_registry_sparql_ukhpi.py` query the SPARQL endpoint and store extracted triples as turtle files.

> The `address_matching.py` script screens matching approaches to match addresses between the HM Land Registry Price Paid Data and the EPC dataset. It requires the `land_registry_sparql.py` script to be run first to download the Price Paid Data as well as the `download_all_data` method from the [EPC agent](https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-EPCInstantiationAgent/Agents/EnergyPerformanceCertificateAgent/epcdata/datainstantiation/epc_retrieval.py) to download the EPC dataset.
