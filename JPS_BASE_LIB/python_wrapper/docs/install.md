## Prerequisite

- You need Python >=3.8 to run the `twa`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version 11](https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot)

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) before installing the `twa` package. The virtual environment can be created as follows:

`(Windows)`

```cmd
python -m venv twa_venv
twa_venv\Scripts\activate.bat
```

`(Linux)`
```sh
python3 -m venv twa_venv
source twa_venv/bin/activate
```

The above commands will create and activate the virtual environment `twa_venv` in the current directory. The terminal display for new commands will be as follows (to facilitate direct copying and pasting of commands from this documentation into the command line, the virtual environment prefix will be omitted throughout the remainder of the documentation):
```
(twa_venv) $
```

Alternatively, if one wish to use conda for managing environments, please follow the instructions provided on the [conda website](https://conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html).

## Installation via pip

To install the `twa` simply run the following command:

```sh
pip install twa
```

The above command will install the `twa` package including the [`JpsBaseLib`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB) Java library that has been packaged together with the Python code.

## [For developers] Installation from the version-controlled source

This type of installation is only for the developers. To install `twa` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the `TheWorldAvatar\JPS_BASE_LIB\python_wrapper` directory and execute the following commands:
```bash
# build and install
pip install .

# or build for in-place development
pip install -e .

# or use the provided "install_wrapper.sh" convenience script,
# that can create virtual environment and install the twa package in one go
# build and install
install_wrapper.sh -v -i
# or build for in-place development
install_wrapper.sh -v -i -e
```

The above commands will install the `twa` Python package only. To include the [JpsBaseLib](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB) Java library, i.e. the `JPS_BASE_LIB` library, please follow the below steps:

1. Navigate to the `TheWorldAvatar\JPS_BASE_LIB` directory and build the `JPS_BASE_LIB` project:
```bash
mvn clean install -DskipTests
```
2. Go to the `JPS_BASE_LIB/target` directory and copy the main project jar file, `jps-base-lib.jar`, and the entire `lib` folder containing the project dependencies into a temporary directory, let us call it `tmp_JpsBaseLib`.
3. Run the following command in the terminal (with the virtual environment where `twa` is installed activated, e.g. `twa_venv`):
```bash
jpsrm install JpsBaseLib <path_to_the_tmp_JpsBaseLib_directory> --jar jps-base-lib.jar
```
4. After successful installation you can access the `JpsBaseLib` resource (classes and methods) in `twa` by simply importing it:
```python
from twa.resources import JpsBaseLib
```
5. Remove the no longer needed `tmp_JpsBaseLib` directory.

## [For developers] Installing additional java resources

The `twa` project can be easily extended to provide wrapping for other `TheWorldAvatar` java projects. To do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. The `JpsBaseLib pom.xml` file shows an example of how to do it with maven. If you wish to do it for other `TheWorldAvatar` project, simply copy the `maven-jar-plugin` and `maven-dependency-plugin` plugins into the project pom file and add the `net.sf.py4j` dependency. These changes will collate all the project dependencies into the `target\lib` directory and include the required `py4j` package in your project. Once that is done and the project is successfully built, the `twa` resource manager command-line utility, `jpsrm`, can be used to install and register the resource. Here are the steps:

1. Copy the project main jar file and the entire `lib` folder a temporary directory, e.g., `tmp_dir`.
2. Run the following command in the terminal (with the virtual environment where `twa` is installed activated, e.g. `twa_venv`):
```bash
jpsrm install <YourResourceName> <from> --jar JARFILE
```
where `jpsrm` is the `twa` resource manager, `<YourResourceName>` is the name you wish to assign to your java resource, e.g. `JpsBaseLib` in the previous section. The `<from>` argument is the absolute path to the `tmp_dir` with all the java project files and the `--jar` option is used to provide a name of the main jar file, `JARFILE`, to be used for communication.
> **NOTE that the `<YourResourceName>` MUST follow Python's classes names requirements, otherwise it will be impossible to import it in Python.**
3. After the successful installation you can access the resource (classes and methods) in `twa` by simply importing it:
```python
from twa.resources import YourResourceName
```
4. Remove the no longer needed `tmp_dir` directory.

## [For developer] `jpsrm` resource manager

`jpsrm` is a resource manager to effectively manage the Java packages in `twa`. To see all `jpsrm` commands and options, run `jpsrm -h` in the terminal.

> **NOTE** The `jpsrm` includes a developer-only convenience command `devinstall` which will run all the installation steps for the `JpsBaseLib` resource. The command will only work if:
 - the `JPS_BASE_LIB` project is present and was successfully built
 - the `twa` project was installed in a developer mode (-e option) inside the `TheWorldAvatar` repository

Here is how to execute the `devinstall` command:

```bash
# execute devinstall
jpsrm devinstall
```
