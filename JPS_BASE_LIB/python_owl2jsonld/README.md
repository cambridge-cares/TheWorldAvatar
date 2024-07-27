# Description #

The `owl2jsonld.py` module provides a simple Python API for converting OWL files into JSON-LD.

# Installation

You need to install rdflib and rdflib-jsonld to convert OWL to JSON-LD.

## Virtual environment setup

It is highly recommended to use a virtual environment (https://docs.python.org/3/tutorial/venv.html) for the owl2jsonld.py module.
The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

`(Linux)`
```sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $
```

The above commands will create and activate the virtual environment `<venv_name>` in the current directory.


## Installation of required libraries

To install the `rdflib` and `rdflib-jsonld` simply run the following command:

```sh
(<venv_name>) $ pip install rdflib rdflib-jsonld
```

The above command will install the  `rdflib` and `rdflib-jsonld` package so that the owl file can be converted into JSON-LD.


# Requirements #

In order to use the `owl2jsonld.py` tool for converting .owl files into the JSON-LD, we need to use appropriate parameter. 
If we want to convert a single file, we need `-f`. If we want to convert all .owl files in a directory, we need to use `-all` parameter exactly. 
With `-f` parameter exactly, we need to provide the `path/to/source.owl` file followed by the optional `path/to/target.jsonld` file as arguments. If no target is given, source will be considered as the target with .jsonld extension.
Apparently, as the files are not in the same directory of the program file, we need to provide full-path of the files. 
If the `source.owl` file does not exist, the system will produce error. However, the `target.jsonld` will be created during the run-time. 
If the file already exists, it will flush content and populate new content.  

In case to convert `-all` files in a directory, we need provide source directory and target directory.

# Command line interface usage #

## Converter CLI

```bash
Usage:
    Case-1: python owl2jsonld.py -f path/to/source.owl [path/to/target.jsonld]
    Case-2: python owl2jsonld.py -all path/to/source_directory [path/to/target_directory]

Parameter:
-f        It converts a single file. 
            Source should be a valid file and 
            target should have a valid path to file. 
-all      This parameter commands to convert all .owl files at the 
            source directory and output will be put in the 
            target directory with the same name as with .owl.
Path/to/source is mandatory, however path/to/target is optional. 
If it is not given, source with .jsonld will be considered as the target.
```

```bash
# Example to convert .owl to .jsonld
$ python owl2jsonld.py -f C:/Users/someuser/KGs/ontokin/hydrogen-mechanism.owl C:/Users/someuser/KGs/ontokin/hydrogen-mechanism.jsonld
OR
$ python owl2jsonld.py -f C:/Users/someuser/KGs/ontokin/hydrogen-mechanism.owl
OR
$ python owl2jsonld.py -all C:/Users/someuser/Documents/CoMo/Data/test C:/Users/someuser/Documents/CoMo/Data/output
OR
$ python owl2jsonld.py -all C:/Users/someuser/Documents/CoMo/Data/test
```

# Authors #
Md Hanif Seddiqui (mhs62@cam.ac.uk), 18 April 2024