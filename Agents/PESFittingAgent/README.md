# Description #


# Installation #
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Requirements

- You need Python >3.5 to run the `pesfit`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version >=8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `pesfit` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv pesfit_venv
$ pesfit_venv\Scripts\activate.bat
(pesfit_venv) $
```

`(Linux)`
```sh
$ python3 -m venv pesfit_venv
$ source pesfit_venv\bin\activate
(pesfit_venv) $
```

The above commands will create and activate the virtual environment `pesfit_venv` in the current directory.

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `pesfit` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\pesfit* directory and execute the following commands:
```bash
# build and install
(pesfit_venv) $ python -m pip install .

# or build for in-place development
(pesfit_venv) $ python -m  pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` or `install_script_conde.sh` convenience scripts, that can create virtual environment and install the `pesfit` in one go:
```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```
Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(pesfit_venv) $ pytest tests\test_termocalc.py
```

# How to use #



# Authors #
Laura Pascazio (lp521@lp521.cam.ac.uk), 30 September 2021