
# Description #

Zeolite ontology abox writer.

<!-- For the installation and running instructions see file OntoZeolite_user_manual-2.pdf -->

# Ontozeolite KG preparation

## Quick start

To instantiate a copy of the Ontozeolite knowledge graph, you will need:

- The input data (stored in the directory `ontozeolite`)
- Python code (in the directory `python`)
- Control scripts (`*.bat` files in the root directory)
- A running copy of Blazegraph database on a server with an empty namespace

The data generation requires less than 10 GB of hard drive space.

The code uses several external packages. It is recommended to use a virtual environment to install them:

`$ python -m venv <venv_name>`

`$ <venv_name>\Scripts\activate.bat`

`(<venv_name>) $`

Install third-party package `pymatgen`:

`(<venv_name>) pip install pymatgen$`

More information can be found at their [official web-site](https://pymatgen.org/installation.html).

Install third-party package `bibtexparser`.

The BibtexParser library requires version 2+. It has to be loaded from
[development branch](https://github.com/sciunto-org/python-bibtexparser),
and NOT from 'pip install'.
Pip install currently has version 1.3 or 1.4.
Command line to install:

`(<venv_name>) pip install --no-cache-dir --force-reinstall git+https://github.com/sciunto-org/python-bibtexparser@main`

More information can be found at their [official web-site](https://github.com/sciunto-org/python-bibtexparser).
Also download a file with [journal abbreviations](https://github.com/jxhe/bib-journal-abbreviation/blob/master/journals.json)
and save it in the `ontozeolite/biblio/bib2csvp/` directory.

Install Third-party package `entityrdfizer`:

`(<venv_name>) $ pip install entityrdfizer`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer).

Install Third-party package `pyuploader`:

`(<venv_name>) $ pip install pyuploader`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader).

Before instantiation change the SERVER and NAMESPACE
variables in file `ontozeo.bat`
to a valid server address and an empty namespace on that server.
Add a password file for the blazegraph server, if the server requires authentication:
a file `blazedev.auth` in the root directory must contain one line: `username:password`.

Now the entire KG can be generated and uploaded by a single command:

`ontozeo.bat`

# Authors #
Pavlo Rutkevych (pr521@cam.ac.uk), 01 Apr 2024
