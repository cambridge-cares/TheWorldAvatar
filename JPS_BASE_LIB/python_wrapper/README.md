# What is py4jps

Py4jps is a thin Python wrapper for the jps-base-lib project. The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access the Java objects in a Java Virtual Machine.

# Requirements

- You need Python 3.4 - 3.7 to run the py4jps. You can install Python by going to the official Python download page: https://www.python.org/getit/
- (**Developers only**) You need to build the jps-base-lib java project and copy its `jps-base-lib.jar` file and the entire `lib` folder from the `target` directory to the `py4jps\resources\` directory. Non developers should have all jars already packaged with the py4jps project. A convenience script, `install_wrapper.sh`, is provided for developers that automates resources copying and installation. See the script's help for further instructions.

# Installation

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the py4jps installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv py4jps
$ py4jps\Scripts\activate.bat
(py4jps) $
```

`(Linux)`
```sh
$ python3 -m venv py4jps
$ source py4jps\bin\activate
(py4jps) $
```

The above commands will create and activate the virtual environment `py4jps` in the current directory.

## Built distribution installation

This type of installation is recommended for non-developers where the py4jps package is installed by pip from a created py4jps wheel file:
```bash
(py4jps) $ pip install <provided_py4jps_wheel_file_name>.whl
```

## Source distribution installation (for developers)

The installation from py4jps source can be done via the following way:
```bash
# build and install
(py4jps) $ pip install .

# or build for in-place development
(py4jps) $ pip install -e .

# or using the provided "install_wrapper.sh" convenience script, that can create virtual environemnt, copy jar resources and install the py4jps package in one go
# build and install
$ install_wrapper.sh -v -i -r
# or build for in-place development, run "install_wrapper.sh -h" to see other available options
$ install_wrapper.sh -v -i -e -r
```

# How to use

This section explains how to effectively use the `py4jps` package by following the best practices. Note that `py4jps` is a very thin wrapper, thus it does not provide a high level abstraction to the jps-base-lib classes. Therefore, the jps-base-lib java documenation must be consulted with to know which java objects to call in order to perform a desired task.

## Best practices:

`JPSGateway`. It is recommended to have only one JPSGateway object in your Python application. This can be easily achieved by instantiating and starting the JPSGateway() object in a specially designed module e.g. jpsSingleton module and then to import this object instance to any other module that requires it.

`JVM module views`. Py4jps uses internally the [py4j](https://www.py4j.org/index.html) package for accessing the jps-base-lib classes. Py4j allows importing Java packages so that you don't have to type the fully qualified names of the classes you want to instantiate. However, any such import statement is global in Python, e.g. the jvm instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). A convenience method has been added to the py4jps to create the module views:
```python
moduleViewInstance = jps.createModuleView()
```
The module view should be created for each Python module in your application that requires access to the jps-base-lib java classes, followed by any desired import statements. The name of the module view is arbitrary, though it is recommended to name it after your Python module file name to avoid confusion and potential name clashes.

## Example code

`Example application design pattern`. Putting the above recommendations together leads to the following design pattern:

*Example project source files layout*

    .
    ├── ...                         # your other project files (README, LICENSE, etc..)
    └── src                         # your project source files
        ├── jpsSingleton.py         # your singleton module that instantiates and starts the JPSGateway object
        ├── main.py                 # your application main entry point
        ├── app_module1.py          # your application module 1
        └── app_module2.py          # your application module 2, etc..


*Example code*

`File: jpsSingleton.py`

```python
# The purpose of this module is to create and start a single
# JPSGateway object to be used in all of your other modules
#============================================================
from py4jps import JPSGateway

jps = JPSGateway()
jps.start()
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
# get a single JPSGateway instance, jps, from the jpsSingleton
from jpsSingleton import jps

# create a JVM module view for this module and use it to import
# the required java classes
app_module1_view = jps.createModuleView()
jps.importPackages(app_module1_view,"uk.ac.cam.cares.jps.base.query.*")

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
# get a single JPSGateway instance, jps, from the jpsSingleton
from jpsSingleton import jps

# create a JVM module view for this module and use it to import
# the required java classes
app_module2_view = jps.createModuleView()
jps.importPackages(app_module2_view,'uk.ac.cam.cares.jps.base.util.*')

# this function shows how to use jps-bae-lib file util object to read a local file
def doTask2():
    FileUtil = app_module2_view.FileUtil
    # any paths passed to the jps=-base-lib FileUtil object should be absolute paths
    response = FileUtil.readFileLocally('<your_absolute_file_path>')
    return response
#============================================================
```

# Authors #

Daniel Nurkowski
