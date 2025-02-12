# Description #

The `build_kg_index.py` module build inverted index of concepts to the files providing a base directory. It automatically considers files and files under sub-folders. It saves the inverted index in a file called `inverted_index.json`. On the other hand, `load_kg_index.py` loads an index from a saved file and produces bar-chart to see the concept to files association bar-chart.

# Installation

You need to install rdflib and matplotlib for this purpose.

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

To install the `rdflib` and `matplotlib` simply run the following command:

```sh
(<venv_name>) $ pip install rdflib matplotlib
```

The above command will install the  `rdflib` and `matplotlib` packages.


# Requirements #



# Command line interface usage #

## Converter CLI



# Authors #
Md Hanif Seddiqui (mhs62@cam.ac.uk), 23 April 2024