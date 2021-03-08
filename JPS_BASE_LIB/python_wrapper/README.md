# What is py4jps

Py4jps is a thin Python wrapper for the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access the Java objects in a Java Virtual Machine.

# Requirements

- You need Python 3.4 - 3.7 to run the py4jps. You can install Python by going to the official Python download page: https://www.python.org/getit/
- You also need to install a Java environment (version 7 or more recent)

# Installation

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the py4jps installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv py4jps_venv
$ py4jps_venv\Scripts\activate.bat
(py4jps_venv) $
```

`(Linux)`
```sh
$ python3 -m venv py4jps_venv
$ source py4jps_venv\bin\activate
(py4jps_venv) $
```

The above commands will create and activate the virtual environment `py4jps_venv` in the current directory.

## Installation from the wheel

This type of installation is recommended for non-developers where the py4jps package is installed by pip from a created py4jps wheel file:
```bash
(py4jps_venv) $ pip install <provided_py4jps_wheel_file_name>.whl
```

The above command will install the `py4jps` package including the `jpsBaseLib` library that has been packaged together with the wheel file.

## Installation from source (for developers)

The installation from py4jps source can be done via the following way:
```bash
# build and install
(py4jps_venv) $ pip install .

# or build for in-place development
(py4jps_venv) $ pip install -e .

# or using the provided "install_wrapper.sh" convenience script, that can create virtual environment and install the py4jps package in one go
# build and install
$ install_wrapper.sh -v -i
# or build for in-place development
$ install_wrapper.sh -v -i -e
```

The above command will install the `py4jps` package only. In order to include the `jpsBaseLib` library, please see the next section

## Installing additional java resources

The `py4jps` project can be easily extended to provide wrapping for any other java resources. In order to do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. Then, the `py4jps` resource manager command line utility can be used to install and register the resource. Here are the steps:

1. Build the java project you wish to use in `py4jps`. The project must have all its dependencies included, either in the main jar file or in a separate folder with the jar file manifest file containing an appropriate class path entries containing relative paths to its dependencies.
2. Copy the project main jar file and all its dependencies to a temporary directory, let's call it `tmp_dir`.
3. Run the following command in the terminal (with `py4jps_venv` virtual environment activated):
```bash
(py4jps_venv) $ jpsrm install <name> <from> --jar JARFILE
```

where `jpsrm` is the `py4jps` resource manager, `<name>` is the name you wish to assign to your java resource. Note that the `<name>` MUST follow python's classes names requirements, otherwise it will be impossible to import it in python. The `<from>` argument is the absolute path to the `tmp_dir` with all the java project files and the `--jar` option is used to provide a name of the main jar file, `JARFILE`, to be used for communication.

4. After successful installation you can access the resource (classes and methods) in `py4jps` by simply importing it:
```python
from py4jps.resources import yourResourceName
```
5. Remove the non-longer needed `tmp_dir` directory.

To see all `jpsrm` commands and options, run ``jpsrm -h`` in the terminal.

## Installing resources (for developers)

These instructions explain how to install the `JPS_BASE_LIB` library and are useful for developers working directly with the `TheWorldAvatar` repository.

Steps to install the `JPS_BASE_LIB` library:

1. Build the `JPS_BASE_LIB` project
2. Go to the `JPS_BASE_LIB/target` directory and copy the main project jar file, `jps-base-lib.jar`, and the entire `lib` folder containing the project dependencies into a temporary directory, let's call it `tmp_jpsBaseLib`.
3. Run the following command in the terminal (with `py4jps_venv` virtual environment activated):
```bash
(py4jps_venv) $ jpsrm install jpsBaseLib <path_to_the_tmp_jpsBaseLib_directory> --jar jps-base-lib.jar
```
4. After successful installation you can access the `jpsBaseLib` resource (classes and methods) in `py4jps` by simply importing it:
```python
from py4jps.resources import jpsBaseLib
```
5. Remove the non-longer needed `tmp_jpsBaseLib` directory.


Note. The `jpsrm` includes a developer only convenience command `devinstall` which will run all the above steps and install the `jpsBaseLib` resource. The command will only work if:
 - the `JPS_BASE_LIB` project is present and was successfully built
 - the `py4jps` project was installed in a developer mode (-e option)

Here is how to execute the `devinstall` command:

```bash
# execute devinstall
(py4jps_venv) $ jpsrm devinstall
```

# How to use

This section explains how to effectively use the `py4jps` package by following the best practices. Note that `py4jps` is a very thin wrapper, thus it does not provide a high level abstraction to the java classes. Therefore, the java project documentation must be consulted with to know which java objects to call in order to perform a desired task.

## Best practices:

`Resource Gateway Objects`. It is recommended to have only one gateway object in your Python application per java resource you wish to access.  This can be easily achieved by instantiating and starting the resource gateway objects in a specially designed module e.g. `jpsSingletons` and then to import these objects instances to any other module that requires it.

`JVM module views`. Py4jps uses internally the [py4j](https://www.py4j.org/index.html) package for accessing the Java classes. Py4j allows importing Java packages so that you don't have to type the fully qualified names of the classes you want to instantiate. However, any such import statement is global in Python, e.g. the jvm instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). A convenience method has been added to the py4jps to create the module views:
```python
moduleViewInstance = resGateway.createModuleView()
```
The module view should be created for each Python module in your application that requires access to the java resource, followed by any desired import statements. The name of the module view is arbitrary, though it is recommended to name it after your Python module file name to avoid confusion and potential name clashes.

## Example code

`Example application design pattern`. Putting the above recommendations together leads to the following design pattern:

*Example project source files layout*

    .
    ├── ...                         # your other project files (README, LICENSE, etc..)
    └── src                         # your project source files
        ├── jpsSingletons.py        # your singleton module that instantiates and starts the resources gateway objects
        ├── main.py                 # your application main entry point
        ├── app_module1.py          # your application module 1
        └── app_module2.py          # your application module 2, etc..


*Example code*

`File: jpsSingletons.py`

```python
# The purpose of this module is to create and start single
# gateway objects to be used in all of your other modules
#============================================================
from py4jps.resources import jpsBaseLib

jpsBaseLibGW = jpsBaseLib()
jpsBaseLibGW.start()
#============================================================
```

`File: main.py`

```python
# The purpose of this module is to have a single entry point
# to your application
#============================================================
from app_module1 import doTask1
from app_module2 import doTask2

response1 = doTask1()
response2 = doTask2()
```

`File: app_module1.py`

```python
# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons
from jpsSingletons import jpsBaseLibGW

# create a JVM module view for this module and use it to import
# the required java classes
app_module1_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(app_module1_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def doTask1():
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = app_module1_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+'ontokin', True, False)
    response = KGClient.executeQuery(("PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \
                                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>	SELECT ?mechanismIRI \
                                    WHERE	{ ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10"))
    return str(response)
```

`File: app_module2.py`
```python
# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons
from jpsSingletons import jpsBaseLibGW

# create a JVM module view for this module and use it to import
# the required java classes
app_module2_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(app_module2_view,'uk.ac.cam.cares.jps.base.util.*')

# this function shows how to use jps-base-lib file util object to read a local file
def doTask2():
    FileUtil = app_module2_view.FileUtil
    # any paths passed to the jps-base-lib FileUtil object should be absolute paths
    response = FileUtil.readFileLocally('<your_absolute_file_path>')
    return response
#============================================================
```

# Note to developers

 The py4jps aim is to provide Python access to the `TheWorldAvatar` project classes and methods. However, it is important to understand that not all `TheWorldAvatar` classes can be accessed. Namely, **any servlet depending classes can not be instantiated in Python without running the TomCat server first**. Since this has not been tested, it is not guaranteed that running the TomCat server would actually fix the problem. However, this should not be an issue for the py4jps users, given that the main purpose of the wrapper is to use client side `TheWorldAvatar` classes to perform KG queries or updates. In other words, it is not the py4jps purpose to develop the server side code, which should happen in Java.


# Authors #

Daniel Nurkowski
