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

The above command will install the `py4jps` package only. In order to include the [jpsBaseLib](https://github.com/cambridge-cares/TheWorldAvatar/tree/master/JPS_BASE_LIB) library, please see the next section

## Installing additional java resources

The `py4jps` project can be easily extended to provide wrapping for any other java resources. In order to do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. Then, the `py4jps` resource manager command line utility can be used to install and register the resource. Here are the steps:

1. Build the java project you wish to use in `py4jps`. The project must have all its dependencies included, either in the main jar file or in a separate folder with the jar file manifest file containing the appropriate class path entries containing relative paths to its dependencies.
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
5. Remove the no longer needed `tmp_dir` directory.

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
5. Remove the no longer needed `tmp_jpsBaseLib` directory.


Note. The `jpsrm` includes a developer only convenience command `devinstall` which will run all the above steps and install the `jpsBaseLib` resource. The command will only work if:
 - the `JPS_BASE_LIB` project is present and was successfully built
 - the `py4jps` project was installed in a developer mode (-e option) inside the TheWorldAvatar

Here is how to execute the `devinstall` command:

```bash
# execute devinstall
(py4jps_venv) $ jpsrm devinstall
```

# How to use

This section explains how to effectively use the `py4jps` package by following the best practices. Note that `py4jps` is a very thin wrapper, thus it does not provide a high level abstraction to the java classes. Therefore, the java project documentation must be consulted with to know which java objects to call in order to perform a desired task.

## Best practices:

### Python-Java communication

The Python-Java communication, handled by the `py4j` package, happens via the local network sockets. The main interaction point between a Python and Java is provided by the [py4j JavaGateway](https://www.py4j.org/py4j_java_gateway.html#javagateway) class. This class is wrapped within the `py4jps JPSGateway` class for convenience. The `JPSGateway` class is the parent class for any installed java resources that one wishes to access. Although, it is not recommended to use it directly, the `JPSGateway` class can be useful in experimenting with different resources without installing them first, as it allows to change the resources at runtime. The class also accepts any `py4j JavaGateway` arguments as defined by the [py4j API](https://www.py4j.org/py4j_python.html). Please see the [JPSGateway help](https://github.com/cambridge-cares/TheWorldAvatar/blob/master/JPS_BASE_LIB/python_wrapper/py4jps/JPSGateway.py) to learn more.

### Resource gateways

The resource gateway class is automatically created upon the resource installation. The class name is the same as the name of the resource given during the installation. Furthermore, each resource gateway class inherits from the `py4jps JPSGateway` class. Therefore, one should ideally only work with the resource classes rather than with the parent JPSGateway class. It is also recommended to have only one gateway object in your Python application per java resource you wish to access. This can be easily achieved by instantiating and starting the resource gateway objects in a specially designed module e.g. `jpsSingletons` and then to import these objects instances to any other module that requires it.

### JVM module views

Py4j allows importing Java packages so that you don't have to type the fully qualified names of the classes you want to instantiate. However, any such import statement is global in Python, e.g. the jvm instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). A convenience method has been added to the `py4jps` gateway class that allows to create the module views (see [py4j](https://www.py4j.org/advanced_topics.html#importing-packages-with-jvm-views) documentation for further details on module views):
```python
# create the module view
moduleViewInstance = resGateway.createModuleView()
```
where the `resGateway` is the gateway of a resource we wish to create the module view for. Once the module view is created, it can be used to import the desired java classes using the `importPackages` method, e.g:
```python
# import classes into the module view
resGateway.importPackages(moduleViewInstance,"uk.ac.cam.cares.jps.base.query.*")
# once classes are imported, they can be accesses as follows
moduleViewInstance.className
# where the 'className' is a name of the class we wish to access
```
The module view should be created for each Python module in your application that requires access to the java resource, followed by any desired import statements. The name of the module view is arbitrary, though it is recommended to name it after your Python module file name and the parent resource to avoid confusion and potential name clashes.

### Example code:

Putting the above recommendations together leads to the following design pattern:

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
# The purpose of this module is to create and start resource
# gateway objects to be used in all of your other modules
#============================================================
from py4jps.resources import jpsBaseLib

# jpsBaseLib resource gateway
# Example on how to pass optional py4j Gateway parameters
# - Note that the auto_convert Gateway parameter is set to False
#   by default in py4j. This flag enables / disables the automatic
#   conversion of Python objects like sequences, lists, maps etc to Java Objects
#   With the flag False, to call any java method that requires java list,
#   array etc object as an input, one would need to:
#   * create such object first on the Python side using the java.lang package, e.g:
#     ArrayListVar = gateway.jvm.java.lang.ArrayList()
#   * populate the object with the desired values
#     ArrayListVar.add('Item 1')
#     ...
#   * and then use it in the method call
#
#   If the flag is True, the python list, map, etc objects are automatically
#   converter to their java equivalents. Since the automatic conversion is
#   extremely convenient, this option is set to True in py4jps JPSGateway class.
#   Thus setting it to True below is not needed and is used just for the example
jpsBaseLibGW = jpsBaseLib(**{'auto_convert': True})
# Example on how to pass optional py4j Gateway.launch_gateway() parameters
# - Note that although the die_on_exit Gateway.launch_gateway() parameter
#   is set to False by default in py4j, the py4jps JPSGateway.launchGateway() method
#   sets its value to True. This example shows how to override it.
jpsBaseLibGW.launchGateway(**{'die_on_exit': False})

# gateways instances for any other resources...
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
# get the jpsBaseLibGW instance from the jpsSingletons module
from jpsSingletons import jpsBaseLibGW

# create a JVM module view for this module and resource and use it to import
# the required java classes
apm1_jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(apm1_jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def doTask1():
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = apm1_jpsBaseLib_view.KGRouter
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
# get the jpsBaseLibGW instance from the jpsSingletons module
from jpsSingletons import jpsBaseLibGW

# create a JVM module view for this module and use it to import
# the required java classes
apm2_jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(apm2_jpsBaseLib_view,'uk.ac.cam.cares.jps.base.util.*')

# this function shows how to use jps-base-lib file util object to read a local file
def doTask2():
    FileUtil = apm2_jpsBaseLib_view.FileUtil
    # any paths passed to the jps-base-lib FileUtil object should be absolute paths
    response = FileUtil.readFileLocally('<your_absolute_file_path>')
    return response
#============================================================
```

# Note to developers

 The py4jps aim is to provide Python access to the `TheWorldAvatar` project classes and methods. However, it is important to understand that not all `TheWorldAvatar` classes can be accessed. Namely, **any servlet depending classes can not be instantiated in Python without running the TomCat server first**. Since this has not been tested, it is not guaranteed that running the TomCat server would actually fix the problem. However, this should not be an issue for the py4jps users, given that the main purpose of the wrapper is to use the client side `TheWorldAvatar` classes to perform KG queries or updates. In other words, it is not the py4jps purpose to develop the server side code, which should happen in Java.


# Authors #

Daniel Nurkowski
