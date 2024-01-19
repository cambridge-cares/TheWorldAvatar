# Description

The `compchemparser` package provides parsers to convert quantum chemistry log files into a more condensed JSON format. At the moment, only one parser for Gaussian log files is implemented.

# Installation
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `compchemparser` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv <your_venv>
$ your_venv\Scripts\activate.bat
(your_venv) $
```

`(Linux)`
```sh
$ python3 -m venv <your_venv>
$ source your_venv/bin/activate
(your_venv) $
```

The above commands will create and activate the virtual environment `your_venv` in the current directory.

## Installation via pip

To install the `compchemparser` simply run the following command:

```sh
(your_venv) $ python -m pip install compchemparser
```

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `compchemparser` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\thermo\CoMoCompChemParser* directory and execute the following commands:

```bash
# build and install
(your_venv) $ python -m pip install .

# or build for in-place development
(your_venv) $ python -m pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` or `install_script_conda.sh` convenience scripts, that can create virtual environment and install the `compchemparser` in one go:

```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```

Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(your_venv) $ pytest tests
```

# How to use

```bash
Usage:
    ccparse <logFileOrDir> [-n --logExt=<LOG_EXT>]

Options:
    -n                    Suppress parser command output files (json and csv)
    --logExt=<LOG_EXT>    Log files file extension for log file directory input [default: .log]
```

# Authors
Daniel Nurkowski (danieln@cmclinnovations.com)
Angiras Menon