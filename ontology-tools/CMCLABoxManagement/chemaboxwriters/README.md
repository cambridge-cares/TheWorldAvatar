# Description #

The `chemaboxwriters` package provides a simple python API for generating and uploading aboxes describing chemical data. The package aims to simplify and unify chemical data generation and upload in the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. Currently, the package allows generation and upload of the following aboxes: ontospecies, ontocompchem, ontopesscan and ontomops.

# Requirements

- Python >=3.5. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- [Java Runtime Environment version 8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)
- Either [miniconda](https://docs.conda.io/en/latest/miniconda.html) or [miniforge](https://github.com/conda-forge/miniforge/releases) package manager

# Installation #
Currently, the package can only be installed from the source using a custom bash script:


`(Windows)`

Open miniconda / miniforge command prompt, navigate to the `chemaboxwriters` directory and run

```cmd
$ install_script_conda.sh -v -e -i
```

`(Linux)`

Open linux bash terminal and run

```sh
$ install_script_conda.sh -v -e -i
```

The command above will create a separate conda virtual environment, install the `chemaboxwriters` package and all its dependencies. Once the installation is done, activate the newly created environment via the following command:
```sh
$ conda activate chemaboxwriters_venv
```

If you wish to see the install script help text, simply run `install_script_conda.sh -h`.

# Configuration

The `chemaboxwriters` uses the [pyuploader](https://pypi.org/project/pyuploader/) package to make the file server and triple store uploads. Prior to running any abox creation and upload it is necessary to set the appropriate endpoints and other upload options. This is done by creating the config yml file. The config yml file path can be then either passed as an argument during the `chemaboxwriters` call or set via the `ABOXWRITERS_CONFIG_FILE` environment variable. An example config file syntax is presented below:

```yml
# these are the default settings that will be applied to any file uploads
# in the upload_to_file_server and upload_to_triple_store sections
triple_store_sparql_endpoint: <default_triple_store_sparql_endpoint>
triple_store_secrets_file: <default_triple_store_secrets_file>
file_server_upload_endpoint: <default_file_server_upload_endpoint>
file_server_secrets_file: <default_file_server_secrets_file>
file_server_subdir: <default_file_server_subdir>
# this section defines what type of files should be uploaded to the file server
upload_to_file_server:
    # this section should be repeated for each input file type you wish to upload
    # below you can see some default options
    qc_log:
        # any file server options defined here would overwrite the default
        # options.
        file_server_upload_endpoint: <file_server_upload_endpoint>
        file_server_secrets_file: <file_server_secrets_file>
        file_server_subdir: ontocompchem
    ominp_xyz:
        # ontomops xyz data
        file_server_upload_endpoint: <file_server_upload_endpoint>
        file_server_secrets_file: <file_server_secrets_file>
        file_server_subdir: ontomops
upload_to_triple_store:
    # this section should be repeated for each input file type you wish to upload
    # below you can see some default options
    oc_owl:
        # ontocompchem abox
        # any triple store options defined here would overwrite the default
        # options.
        triple_store_sparql_endpoint: <triple_store_sparql_endpoint>
        triple_store_secrets_file: <triple_store_secrets_file>
    os_owl:
      # ontospecies abox
        triple_store_sparql_endpoint: <triple_store_sparql_endpoint>
        triple_store_secrets_file: <triple_store_secrets_file>
    ops_owl:
      # ontopesscan abox
        triple_store_sparql_endpoint: <triple_store_sparql_endpoint>
        triple_store_secrets_file: <triple_store_secrets_file>
    om_owl:
      # ontomops abox
        triple_store_sparql_endpoint: <triple_store_sparql_endpoint>
        triple_store_secrets_file: <triple_store_secrets_file>
```

# Command line interface #

The command line interface for all the supported abox writers is presented below. A more abox writer specific CLI description can be obtained by running the each of the abox writers with the --help argument.

```bash
ocompchem [CommonOptions]   # runs the ontocompchem abox writer
ospecies  [CommonOptions]   # runs the ontospecies abox writer
omops     [CommonOptions]   # runs the ontomops abox writer
opesscan  [CommonOptions]   # runs the ontopesscan abox writer
          [OpsscanOptions]

CommonOptions:
--file-or-dir                  Path to the input file or directory
--inp-file-type=<type>         Types of the allowed input files.
                               Allowed types and the default value
                               depends on the abox writer type.
--file-ext=<ext>               Extensions of the input files,
                               specified as a comma separated
                               string, e.g. --file-ext="ext1,ext2"
                               Default value depends on the abox
                               writer type.
--config-file=<file>           Path to the config file specifying upload
                               options. If not provided, the code will
                               try to read the config file path from
                               the ABOXWRITERS_CONFIG_FILE environment
                               variable
--out-dir=<dir>                Output directory to write the
                               abox files to. If not provided
                               defaults to the directory of the
                               input file.
--log-file-name=<name>         Name of the generated log file.
--log-file-dir=<dir>           Path to the abox writer log file.
                               Defaults to the <file_or_dir> dir.
--no-file-logging              No logging to a file flag.
--dry-run=<dry_run>            Run the abox writer tool in a dry    [default: True]
                               run mode (files are not uploaded).
                               Choose between True / False
--info                         Prints the pipeline info without running it.
--help                         Prints this help message.

OpsscanOptions:
--os-iris=<iri>                OntoSpecies iri associated with the
                               scan points
--os-atoms-iris=<iris>         Comma separated iris of ontospecies
                               atoms defining the scan coordinate
--oc-atoms-ids=<ids>           Positions of atoms in ontocompchem
                               scan point geometries (index starts
                               from one), e.g. "1,2"
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