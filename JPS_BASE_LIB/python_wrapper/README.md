# What is py4jps

`Py4jps` is a thin Python wrapper for the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access Java objects in a Java Virtual Machine.

# Requirements

- You need Python >=3.5 to run the py4jps. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version 11](https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot)

# Installation

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `py4jps` installation. The virtual environment can be created as follows:

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

## Installation via pip

To install the `py4jps` simply run the following command:

```sh
(py4jps_venv) $ pip install py4jps
```

The above command will install the `py4jps` package including the `JpsBaseLib` library that has been packaged together with the code.

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `py4jps` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\JPS_BASE_LIB\python_wrapper* directory and execute the following commands:
```bash
# build and install
(py4jps_venv) $ pip install .

# or build for in-place development
(py4jps_venv) $ pip install -e .

# or use the provided "install_wrapper.sh" convenience script,
# that can create virtual environment and install the py4jps package in one go
# build and install
$ install_wrapper.sh -v -i
# or build for in-place development
$ install_wrapper.sh -v -i -e
```

The above commands will install the `py4jps` package only. To include the [JpsBaseLib](https://github.com/cambridge-cares/TheWorldAvatar/tree/master/JPS_BASE_LIB) library, please see the next section.

## Installing additional java resources

The `py4jps` project can be easily extended to provide wrapping for other `TheWorldAvatar` java projects. To do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. The `JpsBaseLib pom.xml` file shows an example of how to do it with maven. If you wish to do it for other `TheWorldAvatar` project, simply copy the `maven-jar-plugin` and `maven-dependency-plugin` plugins into the project pom file and add the `net.sf.py4j` dependency. These changes will collate all the project dependencies into the `target\lib` directory and include the required `py4j` package in your project. Once that is done and the project is successfully built, the `py4jps` resource manager command-line utility, `jpsrm`, can be used to install and register the resource. Here are the steps:

1. Copy the project main jar file and the entire `lib` folder a temporary directory, e.g., `tmp_dir`.
2. Run the following command in the terminal (with `py4jps_venv` virtual environment activated):
```bash
(py4jps_venv) $ jpsrm install <name> <from> --jar JARFILE
```

where `jpsrm` is the `py4jps` resource manager, `<name>` is the name you wish to assign to your java resource. Note that the `<name>` MUST follow python's classes names requirements, otherwise it will be impossible to import it in python. The `<from>` argument is the absolute path to the `tmp_dir` with all the java project files and the `--jar` option is used to provide a name of the main jar file, `JARFILE`, to be used for communication.

3. After the successful installation you can access the resource (classes and methods) in `py4jps` by simply importing it:
```python
from py4jps.resources import yourResourceName
```
4. Remove the no longer needed `tmp_dir` directory.

To see all `jpsrm` commands and options, run `jpsrm -h` in the terminal.

## Installing resources (for developers)

These instructions explain how to install the `JPS_BASE_LIB` library and are useful for developers working directly with the `TheWorldAvatar` repository.

Steps to install the `JPS_BASE_LIB` library:

1. Build the `JPS_BASE_LIB` project.
2. Go to the `JPS_BASE_LIB/target` directory and copy the main project jar file, `jps-base-lib.jar`, and the entire `lib` folder containing the project dependencies into a temporary directory, let us call it `tmp_JpsBaseLib`.
3. Run the following command in the terminal (with `py4jps_venv` virtual environment activated):
```bash
(py4jps_venv) $ jpsrm install JpsBaseLib <path_to_the_tmp_JpsBaseLib_directory> --jar jps-base-lib.jar
```
4. After successful installation you can access the `JpsBaseLib` resource (classes and methods) in `py4jps` by simply importing it:
```python
from py4jps.resources import JpsBaseLib
```
5. Remove the no longer needed `tmp_JpsBaseLib` directory.

Note. The `jpsrm` includes a developer-only convenience command `devinstall` which will run all the above steps and install the `JpsBaseLib` resource. The command will only work if:
 - the `JPS_BASE_LIB` project is present and was successfully built
 - the `py4jps` project was installed in a developer mode (-e option) inside the `TheWorldAvatar` repository

Here is how to execute the `devinstall` command:

```bash
# execute devinstall
(py4jps_venv) $ jpsrm devinstall
```

# How to use

This section explains how to effectively use the `py4jps` package by following the best practices. Note that `py4jps` is a very thin wrapper, thus it does not provide a high-level abstraction to the java classes. Therefore, the java project documentation must be consulted with to know which java objects to call to perform a desired task.

## Python-Java communication via Python's JPSGateway wrapper class

The Python-Java communication, handled by the `py4j` package, happens via the local network sockets. The main interaction point between a Python and Java is provided by the [py4j JavaGateway](https://www.py4j.org/py4j_java_gateway.html#javagateway) class. This class is wrapped within the `py4jps JPSGateway` class for convenience. The `JPSGateway` class is the parent class for any installed java resources that one wishes to access. **Although, it is not recommended to use it directly**, the `JPSGateway` class can be useful in experimenting with different resources without installing them first, as it allows to change the resources at runtime. Please see the [JPSGateway help](https://github.com/cambridge-cares/TheWorldAvatar/blob/master/JPS_BASE_LIB/python_wrapper/py4jps/JPSGateway.py) to learn more, whereas below, only the most important aspects are covered.

The `py4jps JPSGateway` class can be imported to your project via the following command `from py4jps import JPSGateway`. The class can be used in the following way:

```python
from py4jps import JPSGateway

yourGateway = JPSGateway(resName=yourResName, jarPath=yourResJarPath, **JGkwargs)
```

where `resName` is the name you wish to give to your resource, `yourResJarPath` is the absolute path to the resource main jar file and `**JGkwargs` is a dictionary of any optional `argument: value` pairs one wishes to pass to the [py4j JavaGateway](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.JavaGateway) constructor. Note that if you wish to access an already installed resource through the `JPSGateway` (not recommended), then the `jarPath` argument can be omitted as it can be looked up by the resource name in the resource registry. Also, note that some of the `py4j JavaGateway` constructor arguments are not allowed or should be passed to the `py4j launch_gateway` method instead. If that is the case, the code will print a warning message which shows how to set the desired argument correctly. Please also note that according to the `py4j` documentation, the `gateway_parameters` argument to the `py4j JavaGateway` constructor must have the type of the [py4j GatewayParameters](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.GatewayParameters) object. However, to make it easy for the `py4jps` users, this argument can be passed as a dictionary which then is automatically converted into the `py4j GatewayParameters` object. Here is an example call to the `JPSGateway` constructor with extra `py4j JavaGateway` arguments:

```python
from py4jps import JPSGateway

yourGateway = JPSGateway(**{'eager_load': True, 'gateway_parameters': {'auto_field': True}})
```

Please refer to the [py4j documentation](https://www.py4j.org/py4j_java_gateway.html) for the description of all the possible arguments. The most important and useful settings are set by default in the `py4jps JPSGateway` constructor so a user hardly ever need to pass any arguments in that call. If required, however, the `py4jps JPSGateway` constructor defaults can be overwritten by simply passing their new values. Please also note that the `JPSGateway` constructor only instantiates the `JPSGateway` object, and it DOES NOT instantiate the `py4j JavaGateway`, which only happens in the `py4jps launchGateway` method explained in detail below.

## JPSGateway.launchGateway wrapper method

To start the communication with the Java side, please run the following method on your gateway object:

```python
yourGateway.launchGateway(**LGkwargs)
```

where the `**LGkwargs` argument represents a dictionary of any optional `argument: value` pairs one wishes to pass to the [py4j launch_gateway](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.launch_gateway) method. Please refer to the method documentation for the description of all the possible arguments. The most important and useful settings are set by default in the `py4jps launchGateway` method so a user hardly ever need to pass any arguments in that call. If required, however, the `py4jps launchGateway` method defaults can be overwritten by simply passing their new values.

The code below shows an example `py4jps launchGateway` method call with custom arguments.

```python
# The file paths are exemplary only, please change
# according to your needs
stdout_file_handle = open('D:\\stdout_file.out','w')
sterr_file_handle = open('D:\\stderr_file.out','w')

yourGateway.launchGateway(**{'redirect_stdout':stdout_file_handle,'redirect_stderr':sterr_file_handle})
```

The example above shows how to set the optional `py4j JavaGateway.launch_gateway` arguments: `redirect_stdout` and `redirect_stderr` that might be useful for debugging purposes.

## Accessing Java classes and methods

Once your resource gateway is launched you can access any **public** classes and methods. Here is an example on how to access the `StoreRouter` class:

```python
StoreRouter = yourGateway.gateway.jvm.uk.ac.cam.cares.jps.base.query.StoreRouter
```

As can be seen, the gateway's jvm class serves as an entry point to any java classes and objects you wish to access. Simply provide the fully qualified name of the resource to access it. The `uk.ac.cam.cares.jps.base.query.StoreRouter` path is for example purposes only. **Please note that though accessing objects via their fully qualified names is possible, it is not recommended**. A much simpler way exists, through the java import statements explained in the **JVM module views** section.

It is also important to understand the difference in accessing the **java static and non-static methods**. The former can be done without the object instantiation, e.g.

```python
StoreRouter = yourGateway.gateway.jvm.uk.ac.cam.cares.jps.base.query.StoreRouter
StoreRouter.getStoreClient('http://kb/ontokin', True, False)
```

here the `static getStoreClient` method can be called without the `StoreRouter` instantiation. The non-static methods can only be accessed once the object is instantiated, e.g

```python
# create a FileUtil instance to access its non static getDirectoryFiles method
FileUtil = jpsBaseLib_view.FileUtil()
# call the getDirectoryFiles method
output = FileUtil.getDirectoryFiles(method_arguments)
```

## Resource gateways

The resource gateway class is automatically created upon the resource installation. The class name is the same as the name of the resource given during the installation. Furthermore, **each resource gateway class inherits from the `py4jps JPSGateway` class**. Therefore, **one should ideally only work with the resource classes rather than with the parent JPSGateway class**. It is also recommended to have only one gateway object in your Python application per java resource you wish to access. This can be easily achieved by instantiating and starting the resource gateway objects in a single module and then to import these objects instances to any other module that requires it.

To give a simple example, after installing the `JpsBaseLib` resource, one can import and use it in the following way:
```python
from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib(**JGkwargs)
jpsBaseLibGW.launchGateway(**LGkwargs)
```

Do note that compared to the `JPSGateway` class the `JpsBaseLib` constructor call neither accepts the resource name nor the resource jar path as arguments. This ensures that the resource is properly encapsulated.

## JVM module views

`Py4j` allows importing Java packages so that you do not have to type the fully qualified names of the classes you want to access. However, any such import statement is global in Python, e.g., the `jvm` instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). A convenience method has been added to the `py4jps` gateway class that allows to create the module views (see [py4j](https://www.py4j.org/advanced_topics.html#importing-packages-with-jvm-views) documentation for further details on module views):
```python
# create the module view
moduleViewInstance = resGateway.createModuleView()
```
where the `resGateway` is the gateway of a resource we wish to create the module view for. Once the module view is created, it can be used to import the desired java classes using the `importPackages` method, e.g:

```python
# import classes into the module view
resGateway.importPackages(moduleViewInstance,"uk.ac.cam.cares.jps.base.query.*")
# once classes are imported, they can be accesses as follows
StoreRouter = moduleViewInstance.StoreRouter
```
The module view should be created for each Python module in your application that requires access to the java resource, followed by any desired import statements. The name of the module view is arbitrary, though it is recommended to name it after your parent resource to avoid confusion.

## Python-Java objects translation

It is important to understand that calling the Java methods from Python requires a bit of attention to the type of arguments on the Java side. The simplest case is when the Java method arguments are of any primitive type (e.g. int, float, String, etc..). Then on the Python side, it is enough to pass Python primitives (e.g 5 - int, 3.4 - float, 'a_string', etc..) without even declaring their types. Primitives are automatically converted between Python and Java. Another case is when the Java method arguments are of any collections type (e.g., Array, java.util.List, java.util.Set, etc..). If the `py4j JavaGateway` option `auto_convert` is set to True, then the Python - Java collections objects translation happens automatically. Please see the [py4j documentation](https://www.py4j.org/advanced_topics.html#accessing-java-collections-and-arrays-from-python) for a detailed explanation of what is converted into what. As explained in the **Python-Java communication via Python's JPSGateway wrapper class** section, in `py4jps` the `auto_convert` option is set to True by default so a user does not need to worry about it. Here are some code examples:
```python
# example of calling a Java aMethod1 with an int argument
moduleViewInstance.aMethod1(5)
# example of calling a Java aMethod2 with a float argument
moduleViewInstance.aMethod2(3.4)
# example of calling a Java aMethod3 with a string argument
moduleViewInstance.aMethod3('a_string')
# example of calling a Java aMethod4 with a list of strings argument
# this only works because the auto_convert option is enabled by default in py4jps
moduleViewInstance.aMethod4(['a_string'])
```

However, there could be cases where a Java object of a specific type needs to be created for the method call. As an example, the `FileUtil` class in the `JpsBaseLib` resource has a method called `getDirectoryFiles`. The method simply returns a list of files in a directory that matches the provided file extensions. Here is its Java interface:
```java
public List<File> getDirectoryFiles(File folder, List<String> fileExtensions)
```

As can be seen, the first argument is of type `File`, and the second is of type `List<String>`. Additionally, the method is not static so the `FileUtil` object needs to be instantiated first to be able to access this method. Since the Java `File` object needs to be created on the Python side so that the method can be called with the correct arguments, the question arises on how to do it. Now it is important to understand that the launched resource gateway allows you to not only access the resource classes and methods, but it also allows you to access any default Java classes and methods. This can be done in two ways: either via the `resGateway.gateway.jvm` field (not recommended) or via the created resource `moduleViewInstance` (recommended). Here is an example:

```python
# create and launch the jpsBaseLib gateway
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
# create the separate JVM view
jpsBaseLibGW_view = jpsBaseLibGW.createModuleView()
# import required Java packages into the created JVM view
jpsBaseLibGW.importPackages(jpsBaseLibGW_view,'uk.ac.cam.cares.jps.base.util.*')

# create a FileUtil instance to access its non-static methods
# compare it with the StoreRouter example, where there was no need to add
# () brackets to the router, so we were only retrieving the StoreRouter class
FileUtil = jpsBaseLibGW_view.FileUtil()

# create the required `File folder` argument to be passed to the `getDirectoryFiles` method

# option 1 - NOT RECOMMENDED
# not recommended use of a gateway.jvm class to access the default java classes and methods
# here we are instantiating the java.io.File class
javaFolder = jpsBaseLibGW.gateway.jvm.java.io.File('D:\\my_dir')

# option 2 - RECOMMENDED
# recommended use of JVM view to access default java classes and methods
javaFolder = jpsBaseLibGW_view.java.io.File('D:\\my_dir')

# call the getDirectoryFiles
# Note that the `List<String> fileExtensions` argument can be passed
# as a simple Python list of strings thanks to the enabled auto conversion
fileListArray = FileUtil.getDirectoryFiles(javaFolder, [".txt"])

# The returned object is of Java List<File> type. So, one needs to know a bit
# of Java to extract information. Here is an example:
files_list = []
for i in range(fileListArray.size()):
    files_list.append(fileListArray.get(i).toString())

# An extra example
# If the Java method requires as an input a list of Java objects,
# where the objects are not primitives e.g a list of file objects
# that is returned as an output from the getDirectoryFiles call.
# Then one can simply create such list as follows:

javaFileObj = jpsBaseLibGW_view.java.io.File(path_to_a_folder)
# this creates a Python List which stores a javaFileObj inside
PythonList = [javaFileObj]

# or

# this creates a javaArray which stores the javaFileObj inside
# this example, however, is not needed thanks to the auto conversion
# so, one can simply pass the PythonList created above to the method
javaArray = jpsBaseLibGW_view.java.util.ArrayList()
javaArray.append(javaFileObj)
```

## Example code:

Putting the above recommendations together leads to the following project layout:

*Example project source files layout*

    .
    ├── ...                         # other project files (README, LICENSE, etc..)
    └── projectname                 # project source files
        ├── kgoperations            # module that deals with the KG communication
        │    ├── __init__.py
        │    ├── query.py           # base query function
        │    ├── ...                # any other kg-related file modules
        │    └── gateways.py        # python-java communication gateways
        ├── app1                    # application module 1
        │   ├── __init__.py
        │   └── app.py
        ├── app2                    # application module 2, etc..
        │   ├── __init__.py
        │   └── app.py
        └── main.py                 # application main entry point

*Example code*

`File: projectname.kgoperations.gateways.py`

```python
# The purpose of this module is to create and start resource
# gateway objects to be used in all of your other modules
#============================================================
from py4jps.resources import JpsBaseLib

# jpsBaseLib resource gateway
# you may also wish to pass any **JGkwargs
jpsBaseLibGW = JpsBaseLib()

# you may also wish to pass any **LGkwargs
jpsBaseLibGW.launchGateway()

# gateways instances for any other resources...
#============================================================
```

`File: projectname.kgoperations.query.py`
```python
# The purpose of this module is to provide a standard api
# interface for querying the KG. Note that the actual
# implementation is dependent on the version of the jps-base-lib
# installed. Always consult the jps-base-lib docs if in doubt.
#============================================================
from projectname.kgoperations.gateways import jpsBaseLibGW
import json

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):

    # perform an example sparql query,
    # see the jps-base-lib docs for further details
    StoreRouter = jpsBaseLib_view.StoreRouter
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    response = json.loads(str(StoreClient.executeQuery(queryStr)))
    return response
#============================================================
```

`File: main.py`

```python
# The purpose of this file is to have a single entry point
# to your application
#============================================================
from projectname.app1.app import doTask1
from projectname.app2.app import doTask2

response1 = doTask1()
response2 = doTask2()
```

`File: app1.app.py`

```python
# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
from projectname.kgoperations.query import querykg

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def doTask1():
    queryStr ="""
    PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>	SELECT ?mechanismIRI
    WHERE	{ ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10
    """
    return querykg(sparqlEndPoint='http://kb/ontokin',queryStr=queryStr)
```

`File: app2.app.py`
```python
# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
from projectname.kgoperations.gateways import jpsBaseLibGW

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,'uk.ac.cam.cares.jps.base.util.*')

# this function shows how to use jps-base-lib file util object
# to read a local file.
def doTask2():
    FileUtil = jpsBaseLib_view.FileUtil
    # any paths passed to the jps-base-lib FileUtil object should be
    # absolute paths. Here the FileUtil object does not need to be
    # instantiated as the readFileLocally method is static
    return FileUtil.readFileLocally('<your_absolute_file_path>')
#============================================================
```

## Package `agentlogging`:

As of `py4jps==1.0.29`, `agentlogging`, which originally placed [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/f290fb98ce746b591d8b8c93cca1e89a409c959e/Agents/utils/python-utils), is also packaged and released as part of this python wrapper. One can import and use as below:
```python
from py4jps import agentlogging

dev_logger = agentlogging.get_logger("dev")
dev_logger.debug("This is a DEBUG statement")
dev_logger.info("This is an INFO statement")

prod_logger = agentlogging.get_logger("prod")
prod_logger.debug("This is a DEBUG statement")
prod_logger.info("This is an INFO statement")
```
For more details, see the [Logging](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Logging) page on the Wiki.

# Note to developers

 The py4jps aim is to provide Python access to the `TheWorldAvatar` project classes and methods. However, it is important to understand that not all `TheWorldAvatar` classes can be accessed. Namely, **any servlet depending classes can not be instantiated in Python without running the TomCat server first**. Since this has not been tested, it is not guaranteed that running the TomCat server would fix the problem. However, this should not be an issue for the `py4jps` users, given that the main purpose of the wrapper is to use the client-side `TheWorldAvatar` code to perform KG queries or updates. In other words, it is not the `py4jps` purpose to develop the server-side code, which should happen in Java.

# Package release

> **NOTE: Before making the package release, please remove all sub-folders and `resources_registry.json` file in `python_wrapper/py4jps/resources` folder to prevent incorrect packing of Java resources. For more information, please refer to this [issue](https://github.com/cambridge-cares/TheWorldAvatar/issues/800).**

Maintainers who package and publish the most up-to-date codes from the `develop` branch handle the distribution of package py4jps on PyPI and Test-PyPI. If you want to release the package independently, i.e. become a maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your Test-PyPI and PyPI account and password
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine

Please create and checkout to a new branch from the newest `develop` branch once these details are ready. The release process can then be started by using the commands below, depending on the operating system you're using. (REMEMBER TO CHANGE THE CORRECT VALUES IN THE COMMANDS BELOW!)

`(Windows)`

```cmd
$ cd \absolute_path_to\TheWorldAvatar\JPS_BASE_LIB\python_wrapper
$ release_py4jps_to_pypi.sh -v x.x.x
```

`(Linux)`
```sh
$ cd /absolute_path_to/TheWorldAvatar/JPS_BASE_LIB/python_wrapper
$ ./release_py4jps_to_pypi.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, change the version number in `JPS_BASE_LIB/python_wrapper/release_py4jps_to_pypi.sh` to the one you used for the script release.
```sh
echo "./release_py4jps_to_pypi.sh -v 1.0.15   - release version 1.0.15"
```

The changes mentioned above should then be committed with the changes performed automatically during the release process, specifically in python script `JPS_BASE_LIB/python_wrapper/py4jps/__init__.py`
```
__version__ = "1.0.15"
```

and `JPS_BASE_LIB/python_wrapper/setup.py`
```
version='1.0.15',
```

Finally, make a Pull Request for the branch and merge it back into the `develop` branch.

# Authors #

Daniel Nurkowski
Jiaru Bai
