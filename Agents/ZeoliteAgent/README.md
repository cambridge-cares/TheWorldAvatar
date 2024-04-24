
# Description #

Zeolite ontology abox writer.

For the installation and running instructions see file OntoZeolite_user_manual-2.pdf


# Authors #
Pavlo Rutkevych (pr521@cam.ac.uk), 01 Apr 2024

# Ontozeolite KG preparation

## Quick start

To instantiate a copy of the Ontozeolite knowledge graph, you will need:

- The input data (stored in the directory `ontozeolite`)
- Python code (in the directory `python`)
- Control scripts (`*.bat` files in the root directory)
- A running copy of Blazegraph database on a server with an empty namespace

The data generation requires less than 10 GB of hard drive space.

The code uses several external packages. It is recommended to use a virtual environment to install them:

=======================




\hspace{1cm}
`$ python -m venv <venv_name>`

\hspace{1cm}
`$ <venv_name>\Scripts\activate.bat`

\hspace{1cm}
`(<venv_name>) $`

Install third-party package `pymatgen`:

\hspace{1cm}
`(<venv_name>) pip install pymatgen$`

More information can be found at their [official web-site](https://pymatgen.org/installation.html).

Install third-party package `bibtexparser`.

The BibtexParser library requires version 2+. It has to be loaded from
[development branch](https://github.com/sciunto-org/python-bibtexparser),
and NOT from 'pip install'.
Pip install currently has version 1.3 or 1.4.
Command line to install:

\hspace{1cm}
`(<venv_name>) pip install --no-cache-dir --force-reinstall git+https://github.com/sciunto-org/python-bibtexparser@main`

More information can be found at their [official web-site](https://github.com/sciunto-org/python-bibtexparser).

Install Third-party package `entityrdfizer`:

\hspace{1cm}
`(<venv_name>) $ pip install entityrdfizer`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer).

Install Third-party package `pyuploader`:

\hspace{1cm}
`(<venv_name>) $ pip install pyuploader`

More details on the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader).

Before instantiation change the SERVER and NAMESPACE
variables in file `ontozeo.bat`
to a valid server address and an empty namespace on that server.
Add a password file for a server, if the server requires authentication:
a file `blazedev.auth` in the parent directory must contain one line: `username:password`.

Now the entire KG generation can be done by a single command:

\hspace{1cm}
`ontozeo.bat`

The individual steps used in this script are described below.

Once fully uploaded, the KG can be queried by SPARQL queries or programmatically.
Example SPARQL queries can be found in `ontozeolite/queries/`.

## Overview
The zeolite knowledge graph (KG) comprises interconnected entities derived from various ontologies. These entities, including ontospecies, units-of-measure, bibo, ontocrystal, and ontozeolite, are instantiated from input data using different parts of the code.

The entire data for the zeolite KG is divided into parts according to the nature of the data:
1. **Bibliography information**: Uses BibTeX file(s) as input data. Output is `onto_bib` KG.
2. **Crystal information**: Uses Crystallographic Information Files (CIF) as input. Output is `cif_twa` KG.
3. **Zeolite-specific information**: Uses various input data in `.json` or `.csv` format, IRIs defined in `onto_bib`, `cif_twa`, and some other external ontologies. Output is `ontozeolite_kg` KG.

Instantiation of the zeolite KG on a Blazegraph server consists of:
1. Preparation of input data
2. Generation of CSV files
3. Generation of OWL files
4. Uploading the data to Blazegraph server

The default directory for the data is `ontozeolite`, and the file structure is as follows:
- `ontozeolite/biblio/bibfiles/`: input data (required)
- `ontozeolite/biblio/csv/`: generated, temporary file
- `ontozeolite/biblio/owl/`: generated, to upload
- `ontozeolite/crystal/data/`: input data (required)
- `ontozeolite/crystal/csv/`: generated, temporary files
- `ontozeolite/crystal/owl/`: generated, to upload
- `ontozeolite/zeolite/data/`: input data (required)
- `ontozeolite/zeolite/csv/`: generated, temporary files
- `ontozeolite/zeolite/owl/`: generated, to upload


## Bibliography Information KG
### Input
- **ontozeolite/biblio/bibfiles/**: individual bib file(s) (one citation per file)
- **ontozeolite/biblio/bibdata_crossref_doi.tex**: a list of bibtex entries
- **ontozeolite/biblio/bibdata_original_pdf.tex**: a list of bibtex entries

### Processing
1. Run `python combine_bib.py`
2. Run `python bib2csv.py`
3. Convert CSV to RDF: `csv2rdf ontozeolite/biblio/csv/onto_bib.csv --csvType=abox`

### Output
- **ontozeolite/biblio/csv/onto_bib.csv**: bibliography information in CSV format
- **ontozeolite/biblio/owl/onto_bib.owl**: OWL file with all bibliography information
- **ontozeolite/biblio/bib_iri_list.csv**: list of bibliography items and corresponding IRIs

The OWL file for the bibliography part of the KG is generated from standard BibTeX files, with each bibliography entry stored as an entity of the `bibo:Document` class. The TBox for `bibo:Document` can be found in the documentation folder: `ontozeolite/docs/20210503_ProvenanceOntologies_jb2197.pptx`.

## Crystal Information KG
### Input
- **a_final_species_nodup.json**: a list of zeolitic materials
- CIF files mentioned in this list produce abox

### Processing
1. Run `python crystalinfo.py`
2. Convert CSV to RDF for each `i` (0...128): `csv2rdf ontozeolite/crystal/csv/cif_twa_i.csv --csvType=abox`

### Output
- **cif_twa_i.csv** (where i=0...128)
- **cif_twa_i.csv.owl** (where i=0...128)
- **cif_iri_list.csv**

The total size of the ABox for crystal information for the zeolitic materials is approximately 3.0 Gb. Due to limitations of the uploader, the data is divided into separate files not exceeding 50 Mb.

## Zeolite KG

There are currently 256 zeolite frameworks and over 1000 materials, each material belongs to a framework. The file size for the KG containing these frameworks and materials is close to 100Mb, so the data is separated into 3 parts with 100, 100, and 56 frameworks, respectively.

### Input

- `a_final_species_nodup.json`
- `ontozeolite/zeolite/data/*.*`
- `cif_iri_list.csv`
- `bib_iri_list.csv`

### Processing

```bash
python csv_maker.py -c all -f 0 -t 100 -o dir
python csv_maker.py -c all -f 100 -t 200 -o dir
python csv_maker.py -c all -f 200 -t 300 -o dir
python csv_merger.py dir
csv2rdf ontozeolite/zeolite/csv/ontozeolite_kg_i.csv --csvType=abox

Output:

- `ontozeolite_kg_0i.csv` (here i=0,1,2).
- `cif_iri_list.csv`

### Generation of OWL files

OWL files are created from CSV files using the [EntityRDFizer](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer) tool. After activating the virtual environment for each CSV file, run:

```bash
csv2rdf path/to/csv/file.csv --csvType=abox

## Upload OWL files to \texttt{Blazegraph}

All upload in done by a single script:

`upload\_cryst.bat`

The upload is implemented using
[pyuploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_uploader)

It allows upload either to a local Blazegraph server, or remote with authentication.