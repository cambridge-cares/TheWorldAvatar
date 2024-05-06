
# Description #

Zeolite ontology abox writer.
More details can be found in the [preprint](https://como.ceb.cam.ac.uk/preprints/321/).

<!-- For the installation and running instructions see file OntoZeolite_user_manual-2.pdf -->

# Ontozeolite KG preparation

## Quick start

The procedure presented below has been tested on Windows `cmd` terminal.

To instantiate a copy of the Ontozeolite knowledge graph, you will need:

- The input data (stored in the directory `Agents/ZeoliteAgent/zeoliteaboxwriteragent/ontozeolite/`).
  The git repository contains only one zeolite material information as a proof of concept.
- Python code (in the directory `python`)
- Control scripts (`*.bat` files in the root directory)
- A running copy of Blazegraph database on a server with an empty namespace

Entire set of data for generation of the ABox can be found in
[TheWorldAvatar repository](https://www.dropbox.com/scl/fo/i750y84mbh1t8u78vyfwt/AJVlAWA9ImXv_pkeAruZ-Rw?rlkey=g5vpqqba7zltcwbgly1qllw23&st=k9k8a6ru&dl=0).
The data generation requires less than 10 GB of hard drive space.

The code uses several external packages. It is recommended to use a virtual environment to install them:

`$ python -m venv <venv_name>`

`$ <venv_name>\Scripts\activate.bat`

`(<venv_name>) $`

Install third-party package `pymatgen`:

`(<venv_name>) $ pip install pymatgen`

More information can be found at their [official web-site](https://pymatgen.org/installation.html).

Install third-party package `bibtexparser`.

The BibtexParser library requires version 2+. It has to be loaded from
[development branch](https://github.com/sciunto-org/python-bibtexparser),
and NOT from 'pip install'.
Pip install currently has version 1.3 or 1.4.
Command line to install:

`(<venv_name>) $ pip install --no-cache-dir --force-reinstall git+https://github.com/sciunto-org/python-bibtexparser@main`

More information can be found at their [official web-site](https://github.com/sciunto-org/python-bibtexparser).
Also download a file with [journal abbreviations](https://github.com/jxhe/bib-journal-abbreviation/blob/master/journals.json)
and save it in the `Agents/ZeoliteAgent/zeoliteaboxwriteragent/ontozeolite/biblio/bib2csvp/` directory.

Install Third-party package `entityrdfizer`. You need the recent version of the code,
which can be installed by downloading the source code, and run the installation script:

<!-- `(<venv_name>) $ pip install entityrdfizer` -->
`
(<venv_name>) $ git clone --no-checkout https://github.com/cambridge-cares/TheWorldAvatar/
(<venv_name>) $ cd TheWorldAvatar
(<venv_name>) $ git sparse-checkout init --cone
(<venv_name>) $ git sparse-checkout set EntiryRDFizer
(<venv_name>) $ git checkout
(<venv_name>) $ cd EntityRDFizer
(<venv_name>) $ pip install .`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer).

Install Third-party package `pyuploader`:

`(<venv_name>) $ pip install pyuploader`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader).

Before instantiation set the following
variables in file `ontozeo.bat`:
- SERVER to a valid server address
- NAMESPACE a namespace on that server
- AUTH if the server requires authentication add a password file for the blazegraph server,
  otherwise leave blank..
  Such file must contain one line: `username:password`. See example in `blazedev.auth`.

Now the entire KG can be generated and uploaded by a single command:

`(<venv_name>) $ ontozeo.bat`

# Authors #
Pavlo Rutkevych (pr521@cam.ac.uk), 01 Apr 2024
