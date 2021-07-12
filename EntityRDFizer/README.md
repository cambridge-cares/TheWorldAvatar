# Description #

The `entityrdfizer` project is designed to convert entities of any domain and their data and metadata into RDF.
It requires the entities and their data to be provided as inputs in an ABox CSV template, that is
filled in with data. A group of ABox CSV template files are provided under the following URL:
https://github.com/cambridge-cares/TheWorldAvatar/tree/master/JPS_Ontology/KBTemplates/ABox

# Installation #
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `entityrdfizer` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv entityrdfizer_venv
$ entityrdfizer_venv\Scripts\activate.bat
(entityrdfizer_venv) $
```

`(Linux)`
```sh
$ python3 -m venv entityrdfizer_venv
$ source entityrdfizer_venv\bin\activate
(entityrdfizer_venv) $
```

The above commands will create and activate the virtual environment `entityrdfizer_venv` in the current directory.

## Installation via pip

To install the `entityrdfizer` simply run the following command:

```sh
(entityrdfizer_venv) $ pip install entityrdfizer
```

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `entityrdfizer` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\EntityRDFizer* directory and execute the following commands:
```bash
# build and install
(entityrdfizer_venv) $ pip install .

# or build for in-place development
(entityrdfizer_venv) $ pip install -e .
```

Alternatively, use the provided `install_rdfizer.sh` convenience script, that can create virtual environment and install the `entityrdfizer` in one go:
```bash
# create the environment and install the project
$ install_rdfizer.sh -v -i
# create the environment and install the project for in-place development
$ install_rdfizer.sh -v -i -e
```
Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(entityrdfizer_venv) $ pytest
# or
(entityrdfizer_venv) $ pytest tests
```

# How to use #

```bash
Usage:
    csv2rdf <csvFileOrDirPath> [--outDir=<OUT_DIR>]

Options:
--outDir=<OUT_DIR>   Output directory path
```

# Authors #
Feroz Farazi (msff2@cam.ac.uk), 17 May 2021