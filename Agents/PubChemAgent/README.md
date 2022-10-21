# PubChem AGent

## Description

The `PubChem Agent` is a simple API to communicate with The World Avatar knowledge graph to query for the requested information and autopopulate the missing data using the PubChem PUG REST API.

## Installation

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Requirements

- You need Python >3.5 to run the `PubChem Agent`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version >=8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

### Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `PubChem agent` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -3.9 -m venv pubchemagent_venv
$ pubchemagent_venv\Scripts\activate.bat
(pubchemagent_venv) $ 
```

`(Linux)`

```sh
$ python3 -m venv pubchemagent_venv
$ source pubchemagent_venv\bin\activate
(pubchemagent_venv) $
```

The above commands will create and activate the virtual environment `pubchemagent_venv` in the current directory.

### Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `PUbChem agent` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\Agents\PubChemAgent* directory and execute the following commands:

```bash
# build and install
(pubchemagent_venv) $ python -m pip install .

# or build for in-place development
(pubchemagent_venv) $ python -m  pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` convenience scripts, that can create virtual environment and install the `PubChemAgent` in one go:

```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```

Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(pubchemagent_venv) $ pytest tests\test_pubchemagent.py
```

## How to use

The calculator can be used as a simple command line tool or as a web agent.

### Command line usage

 The calculator can be run from the command line via the `pubchemagent` command which accepts the following options:

 ```bash
Usage:
    pubchemagent  (--inchi=<inchi>)

Options:
--inchi=<inchi>                     inchi string
```

Example usage that calculates thermodynamic properties of SiH4 species:

```bash
(pubchemagent_venv) $ pubchemagent --inchi='InChI=1/Ar'
```

### Web agent usage

The web agent uses the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project knowledge graph to retrieve most of the species data needed
Only one input is required in the agent main route:

These are:

/api/pubchemagent/query?`inchi=InChI=inchistring`

```bash
inchi=<Inchi>   inchi 
```

In order to use the pubchemagent as a web agent, simply start a server with the following app entry point:

`(Windows)`

```cmd
(pubchemagent_venv) $ set FLASK_APP=pubchemagent\flaskapp\wsgi.py & flask run
```

`(Linux)`

```bash
(pubchemagent_venv) $ export FLASK_APP=pubchem\flaskapp\wsgi.py && flask run
```

## Authors

Ali Naseri ()
Laura Pascazio (lp521@cam.ac.uk)
