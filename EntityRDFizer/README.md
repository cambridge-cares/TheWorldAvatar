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
    csv2rdf <csvFileOrDirPath> --csvType=<type> [--outDir=<dir>] [--csvTbox=<tbox>]

Options:
--csvType=<type> Type of the csv file.
                 Choose one of abox/tbox   [default: abox]
--outDir=<dir>   Output directory path
--csvTbox=<tbox> TBox in csv format to validate the input ABox csv file (for ABox writer only)
```

## csv file format for ABox

The input csv file must have at least 6 columns: A,B,C,D,E,F.
Extra columns are ignored.

The file specified for parameter `--csvTbox` should follow the format in examples
EntityRDFizer/tests/test_tboxes/ontocompchem/

Rows in csv file contain one of the following:

### Ontology description containing prefixes for the TBox and the ABox.
For ABox prefix:
- Col A: ABox file name (actually not used, but col A cannot be empty)
- Col B: "Ontology"
- Col C: http://www.theworldavatar.com/kb/ontospecies for ABox (To be changed accordingly)
- Col D: "base"
- Col E,F are not used.

Fot TBox prefix:
- Col A: not used (Col A cannot be empty)
- Col B: "Ontology"
- Col C: http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl (To be changed accordingly)
- Col D: "http://www.w3.org/2002/07/owl#imports"
- Col E,F are not used.

The ontology prefix in Col C mush end with SLASH (/) or HASH (#).
The full path of entities will be
http://www.theworldavatar.com/ontology/ontospecies/ClassName or
http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#ClassName,
respectively.

### Definition of an instance of class
The name of the instance can be either a full path or relative to the base ontology.
- Col A: short class name for the ontology defined in the TBox, or a full IRI
         of class name for a class from an external ontologies
- Col B: "Instance"
- Col C: The new instance name. It is possible to provide a full IRI
         of the instance together with the ontology defined in base,
- Col D,E,F must be empty.

### Relation between two class instances
- Col A: Subject. An instance name defined earlier in this file, or a full IRI of the instance
- Col B: "Instance"
- Col C: Object. The instance defined before this point or a full IRI of the instance
- Col D: Predicate. Relative name or rull IRI of the triple: Col A  predicate Col C.
- Col E,F are not used.
If the instance of classes A,C are relatile paths then they must be defined before this line.

### Assign data value to an instance
Data type of the instance can be full path, or one of predefined shortcuts:
'string', 'integer', 'float', 'double', 'decimal', 'datetime', 'boolean'.
For the predefined data types it is possible to add the "xsd:" prefix, like
'xsd:string', etc.

- Col A: Full http:// address of the relation
- Col B: "Data Property"
- Col C: instance to assign the value
- Col D is not used
- Col E: value to be assigned
- Col F: data type of the value.

# Authors #
Feroz Farazi (msff2@cam.ac.uk), 17 May 2021