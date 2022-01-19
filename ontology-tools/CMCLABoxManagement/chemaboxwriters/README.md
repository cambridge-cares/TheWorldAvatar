# Description #

The `chemaboxwriters` package provides a simple python API for generating and uploading aboxes describing chemical data. The package aims to simplify and unify chemical data generation and upload in the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. Currently, it supports the following aboxes: ontospecies, ontocompchem, ontopesscan and ontomops.

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

The command above will create a separate conda virtual environment, install the `chemaboxwriters` package and all its dependencies. Once the installation is done, activate the newly create environment via the following command:
```sh
$ conda activate chemaboxwriters_venv
```

If you wish to see the install script help text, simply run `install_script_conda.sh -h`.

# Configuration

The `chemaboxwriters` uses the [pyuploader](https://pypi.org/project/pyuploader/) package to make the file server and triple store uploads. Prior to running any abox creation and upload it is necessary to set the appropriate endpoints. This is done by defining four environment variables:
- KG_FILE_SERVER_SPECS      stores path to the file server specs file containing its upload url
- KG_FILE_SERVER_SECRETS    stores path to the file server secrets file containing its credential in the `user_name:password` form
- TRIPLE_STORE_SPECS        stores path to the triple store specs file containing its base url
- TRIPLE_STORE_SECRETS      stores path to the triple store secrets file containing its credential in the `user_name:password` form

# Command line interface usage #

## Ontospecies CLI

```bash
    ospecies <fileOrDir>  [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]
                          [--log-file-name=<name>]
                          [--log-file-dir=<dir>]
                          [--no-file-logging]
                          [--fs-upload-subdirs=<subdirs>]
                          [--ts-upload-nmsp=<nmsp>]
                          [--dry-run=<dry_run>]
                          [--disable-uploads]
                          [--info]

Options:
--inp-file-type=<type>         Types of the allowed input files
                               to the ospecies abox writer:
                                - quantum calculation log            [default: qc_log]
                                - quantum calculation json           [qc_json]
                                - ontospecies meta json              [os_json]
                                - ontospecies meta csv               [os_csv]
--qc-log-ext=<ext>             Extensions of the quantum
                               calculation log files, defaults
                               to ".log, .g09" if not specified
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
--disable-uploads              Disables file server and triple store
                               uploads. Differes from the --dry-run
                               option in that it does require uploaders
                               env variables to be set to run the
                               pipeline.
--fs-upload-subdirs=<subdirs>  Replaces any default file server
                               subdirs used when uploading files.
                               Use it as follows:
                                 - Set subdirs for all handlers
                                     <subdir>
                                 - Set subdirs for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<subdir>
                                 - Set subdirs for a nested handler
                                     <handler1>.<handler2>:<subdir>
                                 - Set subdirs for multiple handlers
                                   at once
                                     <handler1>:<subdir1>,<handler2>:<subdir2>
                               To see handlers default subdirs
                               use the --info option.
--ts-upload-nmsp=<nmsp>        Replaces any default triple store
                               namespaces used when uploading triples.
                               Use it as follows:
                                 - Set nmsp for all handlers
                                     <nmsp>
                                 - Set nmsp for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<nmsp>
                                 - Set nmsp for a nested handler
                                     <handler1>.<handler2>:<nmsp>
                                 - Set nmsps for multiple handlers at once
                                     <handler1>:<nmsp1>,<handler2>:<nmsp2>
                               To see handlers default nmsp
                               use the --info option.
--info                         Prints the pipeline's info without running it.
```

## Ontocompchem CLI

```bash
    ocompchem <fileOrDir>   [--inp-file-type=<type>]
                            [--out-dir=<dir>]
                            [--qc-log-ext=<ext>]
                            [--log-file-name=<name>]
                            [--log-file-dir=<dir>]
                            [--no-file-logging]
                            [--fs-upload-subdirs=<subdirs>]
                            [--ts-upload-nmsp=<nmsp>]
                            [--dry-run=<dry_run>]
                            [--disable-uploads]
                            [--info]

Options:
--inp-file-type=<type>         Types of the allowed input files
                               to the ocompchem abox writer:
                                 - quantum calculation log         [default: qc_log]
                                 - quantum calculation json        [qc_json]
                                 - ontocompchem meta json          [oc_json]
                                 - ontocompchem meta csv           [oc_csv]
--out-dir=<dir>                Output directory to write the
                               abox files to. If not provided
                               defaults to the directory of the
                               input file
--qc-log-ext=<ext>             Extensions of the quantum
                               calculation log files, defaults
                               to ".log, .g09" if not specified
--log-file-name=<name>         Name of the generated log file.
--log-file-dir=<dir>           Path to the abox writer log file.
                               Defaults to the <file_or_dir> dir.
--no-file-logging              No logging to a file flag.
--dry-run=<dry_run>            Run the abox writer tool in a dry    [default: True]
                               run mode (files are not uploaded).
                               Choose between True / False
--disable-uploads              Disables file server and triple store
                               uploads. Differes from the --dry-run
                               option in that it does require uploaders
                               env variables to be set to run the
                               pipeline.
--fs-upload-subdirs=<subdirs>  Replaces any default file server
                               subdirs used when uploading files.
                               Use it as follows:
                                 - Set subdirs for all handlers
                                     <subdir>
                                 - Set subdirs for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<subdir>
                                 - Set subdirs for a nested handler
                                     <handler1>.<handler2>:<subdir>
                                 - Set subdirs for multiple handlers
                                   at once
                                     <handler1>:<subdir1>,<handler2>:<subdir2>
                               To see handlers default subdirs
                               use the --info option.
--ts-upload-nmsp=<nmsp>        Replaces any default triple store
                               namespaces used when uploading triples.
                               Use it as follows:
                                 - Set nmsp for all handlers
                                     <nmsp>
                                 - Set nmsp for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<nmsp>
                                 - Set nmsp for a nested handler
                                     <handler1>.<handler2>:<nmsp>
                                 - Set nmsps for multiple handlers at once
                                     <handler1>:<nmsp1>,<handler2>:<nmsp2>
                               To see handlers default nmsp use the --info option.
--info                         Prints the pipeline's info without running it.
```

## Ontomops CLI

```bash
    omops <fileOrDir>  [--inp-file-type=<type>]
                       [--out-dir=<dir>]
                       [--log-file-name=<name>]
                       [--log-file-dir=<dir>]
                       [--no-file-logging]
                       [--fs-upload-subdirs=<subdirs>]
                       [--ts-upload-nmsp=<nmsp>]
                       [--dry-run=<dry_run>]
                       [--disable-uploads]
                       [--info]

Options:
--inp-file-type=<type>         Types of the allowed input files
                               to the omops abox writer:
                                - omops input json file           [default: ominp_json]
                                - omops processed json file       [omops_json]
                                - omops meta csv                  [omops_csv]
--out-dir=<dir>                Output directory to write the
                               abox files to. If not provided
                               defaults to the directory of the
                               input file
--log-file-name=<name>         Name of the generated log file.
--log-file-dir=<dir>           Path to the abox writer log file.
                               Defaults to the <file_or_dir> dir.
--no-file-logging              No logging to a file flag.
--dry-run=<dry_run>            Run the abox writer tool in a dry    [default: True]
                               run mode (files are not uploaded).
                               Choose between True / False
--disable-uploads              Disables file server and triple store
                               uploads. Differes from the --dry-run
                               option in that it does require uploaders
                               env variables to be set to run the
                               pipeline.
--fs-upload-subdirs=<subdirs>  Replaces any default file server
                               subdirs used when uploading files.
                               Use it as follows:
                                 - Set subdirs for all handlers
                                     <subdir>
                                 - Set subdirs for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<subdir>
                                 - Set subdirs for a nested handler
                                     <handler1>.<handler2>:<subdir>
                                 - Set subdirs for multiple handlers
                                   at once
                                     <handler1>:<subdir1>,<handler2>:<subdir2>
                               To see handlers default subdirs
                               use the --info option.
--ts-upload-nmsp=<nmsp>        Replaces any default triple store
                               namespaces used when uploading triples.
                               Use it as follows:
                                 - Set nmsp for all handlers
                                     <nmsp>
                                 - Set nmsp for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<nmsp>
                                 - Set nmsp for a nested handler
                                     <handler1>.<handler2>:<nmsp>
                                 - Set nmsps for multiple handlers at once
                                     <handler1>:<nmsp1>,<handler2>:<nmsp2>
                               To see handlers default nmsp
                               use the --info option.
--info                         Prints the pipeline's info without running it. 
```

## Ontopesscan CLI

```bash
    opesscan <fileOrDir>  [(--os-iris=<iri> --os-atoms-iris=<iris> --oc-atoms-ids=<ids>)]
                          [--inp-file-type=<type>]
                          [--qc-log-ext=<ext>]
                          [--out-dir=<dir>]
                          [--log-file-name=<name>]
                          [--log-file-dir=<dir>]
                          [--no-file-logging]
                          [--fs-upload-subdirs=<subdirs>]
                          [--ts-upload-nmsp=<nmsp>]
                          [--dry-run=<dry_run>]
                          [--disable-uploads]
                          [--info]

Options:
--os-iris=<iri>                OntoSpecies iri associated with the
                               scan points
--os-atoms-iris=<iris>         Comma separated iris of ontospecies
                               atoms defining the scan coordinate
--oc-atoms-ids=<ids>           Positions of atoms in ontocompchem
                               scan point geometries (index starts
                               from one), e.g. "1,2"
--inp-file-type=<type>         Types of the allowed input files
                               to the opesscan abox writer. There
                               are two input file categories:
                               * Input files requiring extra
                                 species/atoms iris and positions
                                 input:
                                 - quantum calculation log            [default: qc_log]
                                 - quantum calculation json           [qc_json]
                                 - ontocompchem meta json             [oc_json]
                               * Input files not requiring extra
                                 input:
                                 - ontopesscan meta json              [ops_json]
                                 - ontopesscan meta csv               [ops_csv]
--qc-log-ext=<ext>             Extensions of the quantum
                               calculation log files,
                               if not specified, defaults to
                               ".log,.g03,.g09,.g16"
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
--disable-uploads              Disables file server and triple store
                               uploads. Differes from the --dry-run
                               option in that it does require uploaders
                               env variables to be set to run the
                               pipeline.
--fs-upload-subdirs=<subdirs>  Replaces any default file server
                               subdirs used when uploading files.
                               Use it as follows:
                                 - Set subdirs for all handlers
                                     <subdir>
                                 - Set subdirs for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<subdir>
                                 - Set subdirs for a nested handler
                                     <handler1>.<handler2>:<subdir>
                                 - Set subdirs for multiple handlers
                                   at once
                                     <handler1>:<subdir1>,<handler2>:<subdir2>
                               To see handlers default subdirs
                               use the --info option.
--ts-upload-nmsp=<nmsp>        Replaces any default triple store
                               namespaces used when uploading triples.
                               Use it as follows:
                                 - Set nmsp for all handlers
                                     <nmsp>
                                 - Set nmsp for a handler
                                   all its sub-handlers (if any)
                                     <handler1>:<nmsp>
                                 - Set nmsp for a nested handler
                                     <handler1>.<handler2>:<nmsp>
                                 - Set nmsps for multiple handlers at once
                                     <handler1>:<nmsp1>,<handler2>:<nmsp2>
                               To see handlers default nmsp
                               use the --info option.
--info                         Prints the pipeline's info without running it.
```

# Authors #
Daniel Nurkowski (danieln@cmclinnovations.com)
Angiras Menon