# CrystalAgent

The ABox generation agent uses a Crystal Information File (CIF) to produce
an ABox according to the OntoCrystal ontology.
The csv file can further produce the OWL file using
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
                                Usually this flag is added, but in case of pure XRD database
                                Default value: "CX"
                          "X" - to add simulated XRD spectrum.
                                If "C" flag is not set, then the csv contains only the XRD data.
                                The top level entity belongs to XRDSpectrum class
                          Default value: "CX".

```

## Installation

# Authors #
Pavlo Rutkevych (pr521@cam.ac.uk), 01 Apr 2024





