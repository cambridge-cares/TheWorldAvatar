# This docment is used as a guideline for setting up py4jps package under a new workdir with a python IDE, like spyder, pycharm
# About what is py4jps, please refer to the ..\TheWorldAvatar\JPS_BASE_LIB\python_wrapper

## Set up the py4jps in your local clone ..\TheWorldAvatar\JPS_BASE_LIB\python_wrapper following the README.md in ..\TheWorldAvatar\JPS_BASE_LIB\python_wrapper.

## Set up Virtual environment in terminal under the *workdir* of the developer 

`(Windows)`

```cmd
$ python -m venv py4jps_venv
$ py4jps_venv\Scripts\activate.bat
(py4jps_venv) $
```
## Installation via pip

To install the `py4jps` simply run the following command:

```sh
(py4jps_venv) $ pip install py4jps
```

## Copy py4jps and docopt.py under the workdir
From the ..\py4jps_venv\Lib\site-packages, copy the py4jps and docopt.py to the workdir.
Delete the JpsbaseLib and resource_regustry.json under ..workdir\py4jps\resources.

## Installing additional java resources under workdir in the cmd terminal

The `py4jps` project can be easily extended to provide wrapping for other `TheWorldAvatar` java projects. To do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. The `JpsBaseLib pom.xml` file shows an example of how to do it with maven. If you wish to do it for other `TheWorldAvatar` project, simply copy the `maven-jar-plugin` and `maven-dependency-plugin` plugins into the project pom file and add the `net.sf.py4j` dependency. These changes will collate all the project dependencies into the `target\lib` directory and include the required `py4j` package in your project. Once that is done and the project is successfully built, the `py4jps` resource manager command-line utility, `jpsrm`, can be used to install and register the resource. Here are the steps:

1. Copy the project main jar file and the entire `lib` folder a temporary directory, e.g., `tmp_dir`.
2. Run the following command in the terminal (with `py4jps_venv` virtual environment activated):
```bash
(py4jps_venv) $ jpsrm install <name> <from> --jar JARFILE
```

name: JpsBaseLib
from: full path of `tem_dir`
JARFILE: jps-base-lib.jar
e.g. jpsrm install JpsBaseLib C:\Users\wx243\Documents\TheWorldAvatar\JPS_BASE_LIB\python_wrapper\tem_dir --jar jps-base-lib.jar
