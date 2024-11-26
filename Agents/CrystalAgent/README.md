# CrystalAgent

The ABox generation agent uses a Crystal Information File (CIF) to produce
an ABox according to the OntoCrystal ontology.
The csv file can further used to generate the OWL file using
[EntityRdfizer](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer).

The output includes: the csv file(s) containing the abox, and a csv file
`cif_iri_list.csv` containing the following data:
CIF_PATH,CIF_IRI,XRD_PATH,XRD_IRI

```bash
Usage:
python python/crystalinfo.py --inputCIF=<path> [--aboxPrefix=<aPrefix>] [--outDir=<>] [--xrdDir] [--flag=]

Options:
--inputCIF=<path>         CIF file path or a directory containing CIF files.
                          Required argument.
--outDir=<dir>            Output directory path (optional)
                          Default: save directory as the original CIF file.
--xrdDir=<xrd>            Folder to store the simulated xrd peaks (optional).
                          Used only is X flag is present in the --flags argument.
                          Contains the file with ".xrd" extension.
                          Warning: if the ".xrd" file is already present in the <xrd> directory,
                          it is not recomputed.
--aboxPrefix=<aPrefix>    The prefix for entities of the classes (optional).
                          For example: https://www.theworldavatar.com/kg/cifdata/.
                          Default value: ""
--flags=<FLAG>            A string containing letters used as flags. Allowed letters:
                          "C" - top level is an instance of the CrystalInformation class.
                                Usually this flag is added, but may be omitted in case of pure XRD database (see flag "X" below).
                          "U" - to add the basic Unit Cell paraters: a,b,c,alpha,beta,gamma,volume,symmetry.
                                Requires the "C" flag.
                          "V" - to add unit cell vectors.
                                These can be computed from the Unit Cell parameters.
                                Requires the "C" flag.
                          "R" - to add data related Reciprocal unit cell: parameters,unit cell vectors.
                                These can be computed from the Unit Cell parameters.
                                Requires the "C" flag.
                          "T" - to add coordinate Transformation matrix from Fractional to Cartesian and vice versa.
                                These can be computed from the Unit Cell parameters.
                                Requires the "C" flag.
                          "A" - to add coordinates of atom sites in the unit cell.
                                Requires the "C" flag.
                          "X" - to add simulated XRD spectrum.
                                If "C" flag is not set, then the csv contains only the XRD data.
                                The top level entity belongs to XRDSpectrum class.
                          Default value: "CUVRTAX".

```

## Installation

### Virtual environment
It is recommended to use a virtual environment to install packages:

```bash
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

### Install package `pymatgen`

`(<venv_name>) $ pip install pymatgen`

More information can be found at the [official web-site](https://pymatgen.org/installation.html).

### Install package `entityrdfizer` (optional)
If you need to convert later the ABox csv into OWL, you can install the converter:

`(<venv_name>) $ pip install entityrdfizer`

More details at the [TWA web-site](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/EntityRDFizer).

On some systems a message `ModuleNotFoundError: No module named 'pkg_resources'` may appear.
Try to install additional package `pip install setuptools` to fix this issue.

### Install package `Dans_Diffraction`.
This package is required for the simulated XRD spectra.

`(<venv_name>) $ pip install Dans-Diffraction`

Check more details at the [official web-site](https://pypi.org/project/Dans-Diffraction/).


# Authors #
Pavlo Rutkevych (pr521@cam.ac.uk), 01 Apr 2024
