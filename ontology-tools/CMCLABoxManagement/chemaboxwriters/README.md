# Description


The `chemaboxwriters` package provides a simple python API for generating and uploading aboxes, describing chemical data, to the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) knowledge graph. The package's main aim is to simplify and unify the chemical data abox generation and upload steps by supporting variety of different input formats and abox types and by allowing an easy configuration. Currently, the following aboxes are supported:
- `ontospecies`
- `ontocompchem`
- `ontopesscan`
- `ontomops`


## Abox writers overview

All of the listed abox writers were built in a modular fashion as an input processing pipeline containing a number of different handlers, where each handler is specialised in processing a particular type of input. All handlers are tagged with the input and output stages determining supported type of input (file types) and produced output type. Once the handler processes the input it passes the generated output to another handler that is able to process it. Thanks to such design, all abox writers are capable of starting with any input type as long as one of its handlers supports it.

Running any of the supported abox writers results in the following steps:
1. assemble the pipeline
2. configure the pipeline handlers for the upload and query operations
3. check the inputs and pass it to a handler that supports it
4. handler: receives the inputs
5. handler: if configured, uploads the inputs to the specified endpoints
6. handler: handles the inputs and produces the outputs
7. handler: if configured, uploads the outputs to the specified endpoints
8. Repeat steps 2-4 until the end of the pipeline is reached


### OntoSpecies abox writer

The ontospecies abox writer creates and uploads the ontospecies aboxes. The writer is schematically presented on the figure below. It consists of four handlers:
- QC_LOG_TO_QC_JSON
  - input type: QC_LOG - the quantum calculation log files (currently only Gaussian G03, G06, G09, G16 are supported)
  - output type: QC_JSON - a generic quantum calculations json file
- QC_JSON_TO_OS_JSON
  - input type: QC_JSON
  - output type: OS_JSON - an intermediate ontospecies meta json file
- OS_JSON_TO_OS_CSV
  - input type: OS_JSON
  - output type: OS_CSV - an intermediate ontospecies csv file
- OS_CSV_TO_OS_OWL
  - input type: OS_CSV
  - output type: OS_OWL - the ontospecies owl file


<a>
  <center>
    <img src="ospecies.svg" width="600">
  </center>
</a>

### OntoCompChem abox writer

The ontocompchem abox writer creates and uploads the ontocompchem aboxes. The writer is schematically presented on the figure below. It consists of four handlers:
- QC_LOG_TO_QC_JSON
  - input type: QC_LOG
  - output type: QC_JSON - a generic quantum calculations json file
- QC_JSON_TO_OC_JSON
  - input type: QC_JSON
  - output type: OC_JSON - an intermediate ontocompchem meta json file
  - note that this handler attempts to query the ontospecies endpoint (if configured) for the ontospecies IRI (using the species inchi string) so that a given ontocompchem job can be linked to it. If such linking is desired, it is advised to first check if the corresponding ontospecies entry exists at the configured endpoint before running the ontocompchem writer. If the ontospecies entry does not exists the ontospecies abox writer can be used to quickly to create it
- OC_JSON_TO_OC_CSV
  - input type: OC_JSON
  - output type: OC_CSV - an intermediate ontocompchem csv file
- OC_CSV_TO_OC_OWL
  - input type: OC_CSV
  - output type: OC_OWL - the ontocompchem owl file

<a>
  <center>
    <img src="ocompchem.svg" width="600">
  </center>
</a>

### OntoPESScan abox writer

The ontopesscan abox writer creates and uploads the ontopesscan aboxes. The writer is schematically presented on the figure below. It consists of three handlers:


- OC_JSON_TO_OPS_JSON
  - input type: OC_JSON
  - output type: OPS_JSON - an intermediate ontopesscan json file
  - note that running the ontopesscan abox writer at OC_JSON stage requires an additional user input:
    - ontospecies IRI that this scan is about
    - ontospecies atoms IRIs defining the scan coordinate (two IRIs - bond scan, three IRIs - angle scan and four IRIs - dihedral angle scan)
    - atoms indices (starting from one) in the ontocompchem entries corresponding to the indicated ontospecies atoms, e.g. for the bond scan jobs between the ontospecies "atom1_iri,atom2_iri" this input should provide these atoms positions in the ontocompchem jobs reference frame, e.g. "3,4" in case "atom1_iri,atom2_iri" where located at position 3 and 4
- OPS_JSON_TO_OPS_CSV
  - input type: OPS_JSON
  - output type: OPS_CSV - an intermediate ontopesscan csv file
- OPS_CSV_TO_OPS_OWL
  - input type: OPS_CSV
  - output type: OPS_OWL - the ontopesscan owl file


<a>
  <center>
    <img src="opsscan.svg" width="600">
  </center>
</a>

As can be seen from the handlers description, the ontopesscan abox writer relies on the input obtained from the other abox writers. The typical workflow for creating and uploading the opsscan aboxes is as follows:
- select a chemical species to run the scan jobs on
- create the chemical species ontospecies entry using the ontospecies provided abox writer
- run the quantum calculation scan jobs (e.g. bond, angle, dihedral angle scans) on the selected species. Currently, only Gaussian jobs are supported.
- create and upload the ontocompchem aboxes using the provided ontocompchem abox writer
- use the intermediate ontocompchem OC_JSON files as an input for the ontopesscan abox writer.

### OntoMops abox writer

The ontomops abox writer creates and uploads the ontomops aboxes. The writer is schematically presented on the figure below. It consists of three handlers:
- OMINP_JSON_TO_OM_JSON
  - input type: OMINP_JSON - an ontomops input json file
  - output type: OM_JSON - an intermediate ontomops json file
- OM_JSON_TO_OM_CSV
  - input type: OM_JSON - an intermediate ontomops json file
  - output type: OM_CSV - an intermediate ontomops csv file
- OM_CSV_TO_OM_OWL
  - input type: OM_CSV - an intermediate ontomops csv file
  - output type: OM_OWL - the ontomops owl file


<a>
  <center>
    <img src="opsscan.svg" width="600">
  </center>
</a>


Please see the package `tests` folder for the examples of all the supported input files.

# Requirements

- Python >=3.5. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- [Java Runtime Environment version 8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)
- Either [miniconda](https://docs.conda.io/en/latest/miniconda.html) or [miniforge](https://github.com/conda-forge/miniforge/releases) (recommended) package manager

# Installation #
Currently, only the installation from source is possible. A custom bash script has been provided in order to simplify this step:


`(Windows)`

Open miniconda / miniforge command prompt, navigate to the `chemaboxwriters` directory and run:

```cmd
$ install_script_conda.sh -v -e -i
```

`(Linux)`

Open linux bash terminal and run:

```sh
$ install_script_conda.sh -v -e -i
```

The command above will create a separate conda virtual environment `chemaboxwriters_venv`, install the `chemaboxwriters` package and all its dependencies. Once the installation is done, activate the newly created environment via the following command:

```sh
$ conda activate chemaboxwriters_venv
```

If you wish to see the install script help text, simply run `install_script_conda.sh -h`.

# Configuration

Prior to running any abox creation and upload it is necessary to configure appropriate upload and query endpoints. This is done via the config yml file. The config yml file path can be then either passed as an argument during the `chemaboxwriters` call or set via the `ABOXWRITERS_CONFIG_FILE` environment variable. An example config file can be found in the `chemaboxwriters` repository and is also presented below.

```yml
# ------------------------------------------------------------------------------
# This is the aboxwriters config file that is used to specify all the upload
# and query settings. All the settings can be specified at three different
# levels of granularity:
#
# - default settings  - any settings defined here would apply to all
#                       pipelines handlers
# - pipeline settings - any settings defined here would apply to all
#                       handlers for the selected pipeline and would
#                       overwrite any default settings
# - handler settings  - any settings defined here would apply only
#                       to the selected pipeline handler and would
#                       overwrite any default and pipeline level settings
# ------------------------------------------------------------------------------
# DEFAULTS
# ------------------------------------------------------------------------------
triple_store_sparql_endpoint: default_triple_store_sparql_endpoint
triple_store_secrets_file: default_triple_store_secrets_file
file_server_upload_endpoint: default_file_server_upload_endpoint
file_server_secrets_file: default_file_server_secrets_file
file_server_subdir: default_file_server_subdir
ospecies_query_endpoint: ospecies_query_endpoint
omops_query_endpoint: omops_query_endpoint
ocompchem_query_endpoint: ocompchem_query_endpoint
opsscan_query_endpoint: opsscan_query_endpoint
#
# These settings define which files would be uploaded to the file server and
# or to the triple store by indicating the stages they belong to. These are
# default global settings which would propagate to all defined pipelines and
# their handlers unless overwritten.
#
# upload_to_file_server:
#     - test_stage1
# upload_to_triple_store:
#     - test_stage2
# ------------------------------------------------------------------------------
# PIPELINES
# ------------------------------------------------------------------------------
#
# OCOMPCHEM
#-----------------------------------------
ocompchem:
    # these options would overwrite any default settings above
    file_server_subdir: ontocompchem
    triple_store_sparql_endpoint: ontocompchem
    #-----------------------------------------
    # OCOMPCHEM HANDLERS
    #-----------------------------------------
    # these options define any handler specific settings and would overwrite
    # any pipeline and default settings
    handlers:
        qc_log_to_qc_json:
            upload_to_file_server:
                - qc_log
        oc_csv_to_oc_owl:
            upload_to_triple_store:
                - oc_owl
# OSPECIES
#-----------------------------------------
ospecies:
    file_server_subdir: ospecies
    triple_store_sparql_endpoint: ospecies
    #-----------------------------------------
    # OSPECIES HANDLERS
    #-----------------------------------------
    handlers:
        os_csv_to_os_owl:
            upload_to_triple_store:
                - os_owl
# OMOPS
#-----------------------------------------
omops:
    file_server_subdir: omops
    triple_store_sparql_endpoint: omops
    #-----------------------------------------
    # OMOPS HANDLERS
    #-----------------------------------------
    handlers:
        ominp_json_to_om_json:
            upload_to_file_server:
                - ominp_xyz
        om_csv_to_om_owl:
            upload_to_triple_store:
                - om_owl
# OPSSCAN
#-----------------------------------------
opsscan:
    file_server_subdir: opsscan
    triple_store_sparql_endpoint: opsscan
    #-----------------------------------------
    # OPSSCAN HANDLERS
    #-----------------------------------------
    handlers:
        ops_csv_to_ops_owl:
            upload_to_triple_store:
                - ops_owl
# ------------------------------------------------------------------------------
```

# Command line interface #

The command line interface for all the supported abox writers is presented below. A more abox writer specific CLI description can be obtained by running the each of the abox writers with the --help argument.

```bash
Usage:
   aboxwriter ospecies   [options]
   aboxwriter ocompchem  [options]
   aboxwriter omops      [options]
   aboxwriter opsscan    [options]
                         [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]

Options:
--help                  Prints this help message.
--file-or-dir=<dir>     Path to the input file or directory.
--inp-file-type=<type>  Types of the allowed input files to the:
                          * ospecies aboxwriter
                            - quantum calculation log (defualt)  [qc_log]
                            - quantum calculation json           [qc_json]
                            - ontospecies meta json              [os_json]
                            - ontospecies meta csv               [os_csv]
                          * ocompchem aboxwriter
                            - quantum calculation log (defualt)  [qc_log]
                            - quantum calculation json           [qc_json]
                            - ontocompchem meta json             [oc_json]
                            - ontocompchem meta csv              [oc_csv]
                          * omops aboxwriter
                            - omops input json file (defualt)    [ominp_json]
                            - omops processed json file          [omops_json]
                            - omops meta csv                     [omops_csv]
                          * opsscan aboxwriter
                            Inputs that require the extra
                            (--os-iris, --os-atoms-iris
                             --oc-atoms-ids)
                            - ontocompchem meta json (defualt)   [oc_json]
                            - ontopesscan meta json              [ops_json]
                            - ontopesscan meta csv               [ops_csv]
--file-ext=<ext>        Extensions of the input files,
                        specified as a comma separated
                        string, e.g. --file-ext="out,log"
                        if not provided, defaults to the
                        following values:
                         - qc_log stage:
                           "qc_log,log,out,g03,g09,g16"
                         - for all other stages
                           the extension equals to the
                           input file type (e.g. oc_json)
--config-file=<file>    Path to the config file with the upload
                        options. If not provided, the code will
                        try to read the config file path from
                        the ABOXWRITERS_CONFIG_FILE environment
                        variable.
--out-dir=<dir>         Output directory to write the
                        abox files to. If not provided
                        defaults to the directory of the
                        input file.
--log-file-name=<name>  Name of the generated log file.
--log-file-dir=<dir>    Path to the abox writer log file.
                        Defaults to the <file_or_dir> dir.
--no-file-logging       No logging to a file flag.
--dry-run=<dry_run>     Run the abox writer tool in a dry        [default: True]
                        run mode (files are not uploaded).
                        Choose between True / False
--info                  Prints the pipeline info.
--os-iris=<iri>         OntoSpecies iri associated with the
                        scan points. Only required for the
                        opsscan command run with the
                        "oc_json" input file type.
--os-atoms-iris=<iris>  Comma separated iris of ontospecies
                        atoms defining the scan coordinate.
                        Only required for the opsscan
                        command run with the "oc_json" input
                        file type.
--oc-atoms-ids=<ids>    Positions of atoms in ontocompchem
                        scan point geometries (index starts
                        from one), e.g. "1,2". Only required
                        for the opsscan command run with the
                        "oc_json" input file type.
```

# Example usage #

Throughout this section the following alias will be used:

```bash
<aboxwriter> = ocompchem, ospecies, omops, opesscan
```

1. Printing the abox writer info. Use it to check if the abox writer has been correctly configured (especially the upload settings)

```bash
# this reads the config file path from the ABOXWRITERS_CONFIG_FILE env variable
<aboxwriter> --info
# config file path explicitly passed
<aboxwriter> --info --config-file config_file_path
```

2. Running the abox writer on a single file in dry-run mode (default)
```bash
# this will run the abox writer on file1.ext1
<aboxwriter> --file-or-dir file1.ext1
```

3. Running the abox writer on a single file in non dry-run mode. This will upload any output files to appropriate endpoints as specified in the config yml file.

```bash
<aboxwriter> --file-or-dir file1.ext1 --dry-run FALSE
```

4. Running the abox writer on a single file in non dry-run mode while specifying the file type. This will upload any output files to appropriate endpoints as specified in the config yml file.

```bash
# --inp-file-type argument simply sets the first processing stage in the abox writer
<aboxwriter> --file-or-dir file1.ext1 --inp-file-type type --dry-run FALSE
```

5. Running the abox writer on a directory in non dry-run mode. This will upload any output files to appropriate endpoints as specified in the config yml file.

```bash
# depending on the abox writer, different --inp-file-type and --file-ext will be assumed
# the files in the directory will be then picked based on the default --file-ext
# and processed starting from the --inp-file-type stage
<aboxwriter> --file-or-dir my_dir --dry-run FALSE
```

6. Running the abox writer on a directory in non dry-run mode while specifying the file type and extension. This will upload any output files to appropriate endpoints as specified in the config yml file.

```bash
# the --file-ext argument is useful if your file extensions are different than the abox writer defaults
# the files in the directory will be then picked based on the passed --file-ext
# and processed starting from the --inp-file-type stage
<aboxwriter> --file-or-dir my_dir --inp-file-type type --file-ext ext1 --dry-run FALSE
```

7. Running the opesscan abox writer on a single Gaussian log file (qc_log stage) in a non dry-run mode. In case of the qc_log, qc_json and oc_json type inputs, three additional arguments are required. These are --os-iris, --os-atoms-iris and --oc-atoms-ids. For a simple ethanol C1-C2 scan, the --os-iris must be set to the iri of the ethanol in ontospecies triple store, the --os-atoms-iris must be set to the ethanol C1 and C2 atoms iris in the ontospecies triple store and --oc-atoms-ids must be set to the C1 and C2 atoms indices according to the order used in the quantum calculation job. If, e.g. the atom C1 and C2 order was 2 and 3 in the log file the --oc-atoms-ids must be set to "2,3". Please also note that running the opesscan abox writer on qc_log, qc_json and oc_json stages will result in creation and upload of the ontocompchem aboxes as well as the ontopescan aboxes.

```bash
opesscan --file-or-dir my_scan.log
         --os-iris "http://example_ontospecies_iri"
         --os-atoms-iris "http://example_ontospecies_iri/atom_C1_iri,http://example_ontospecies_iri/atom_C2_iri"
         --oc-atoms-ids = "2,3"
         --dry-run FALSE
```



# Authors #
Daniel Nurkowski (danieln@cmclinnovations.com)
Angiras Menon