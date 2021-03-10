# What is py4jps

`Py4jps` is a thin Python wrapper for the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access the Java objects in a Java Virtual Machine.

# Requirements

- You need Python 3.4 - 3.7 to run the py4jps. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version 8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

# Installation

These instructions will get you a copy of the project up and running on your local machine for the development and testing purposes.

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

## Installation from the wheel

This type of installation is recommended for non-developers where the `py4jps` package is installed by pip from the provided wheel file:
```bash
(py4jps_venv) $ pip install <provided_py4jps_wheel_file_name>.whl
```

The above command will install the `py4jps` package including the `jpsBaseLib` library that has been packaged together with the code.

## Installation from the tarball

Another way of installing the `py4jps` is from its tarball. Similarly to the wheel installation, this option is recommended for non-developers where the `py4jps` source code and default `jpsBaseLib` library are zipped together into the final tar.gz file. To install the package simply run:
```bash
(py4jps_venv) $ pip install <provided_py4jps_tarball>.tar.gz
```

## Installation from the version controlled source (for developers)

This type of installation is only for the developers. In order to install `py4jps` directly from its repository you need to firstly clone the `TheWorldAvatar` project. Once that's done simply navigate to the *TheWorldAvatar\JPS_BASE_LIB\python_wrapper* directory and execute the folowing commands:
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

The above commands will install the `py4jps` package only. In order to include the [jpsBaseLib](https://github.com/cambridge-cares/TheWorldAvatar/tree/master/JPS_BASE_LIB) library, please see the next section.

## Installing additional java resources

The `py4jps` project can be easily extended to provide wrapping for other `TheWorldAvatar` java projects. In order to do that, all resource files and their dependencies must be collated into a single directory, with the main jar file located at the directory root. The `jpsBaseLib pom.xml` file shows an example of how to do it with maven. If you wish to do it for other `TheWorldAvatar` project, simply copy the `maven-jar-plugin` and `maven-dependency-plugin` plugins into the project pom file and add the `net.sf.py4j` dependency. These changes will collate all the project depndencies into the `target\lib` directory and include the required `py4j` package in your project. Once that is done and the project is successfully built, the `py4jps` resource manager command line utility, `jpsrm`, can be used to install and register the resource. Here are the steps:

1. Copy the project main jar file and the entire `lib` folder a temporary directory, e.g. `tmp_dir`.
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
 - the `py4jps` project was installed in a developer mode (-e option) inside the `TheWorldAvatar` repository

Here is how to execute the `devinstall` command:

```bash
# execute devinstall
(py4jps_venv) $ jpsrm devinstall
```

# How to use

This section explains how to effectively use the `py4jps` package by following the best practices. Note that `py4jps` is a very thin wrapper, thus it does not provide a high level abstraction to the java classes. Therefore, the java project documentation must be consulted with to know which java objects to call in order to perform a desired task.

## Python-Java communication

The Python-Java communication, handled by the `py4j` package, happens via the local network sockets. The main interaction point between a Python and Java is provided by the [py4j JavaGateway](https://www.py4j.org/py4j_java_gateway.html#javagateway) class. This class is wrapped within the `py4jps JPSGateway` class for convenience. The `JPSGateway` class is the parent class for any installed java resources that one wishes to access. **Although, it is not recommended to use it directly**, the `JPSGateway` class can be useful in experimenting with different resources without installing them first, as it allows to change the resources at runtime. The class also accepts any `py4j JavaGateway` arguments as defined by the [py4j API](https://www.py4j.org/py4j_python.html). Please see the [JPSGateway help](https://github.com/cambridge-cares/TheWorldAvatar/blob/master/JPS_BASE_LIB/python_wrapper/py4jps/JPSGateway.py) to learn more, whereas below, only most important aspects are covered.

The `py4jps JPSGateway` class can be imported to your project via the following command `from py4jps import JPSGateway`. The class can be used to instantiate the gateway objects in a following way:
```python
from py4jps import JPSGateway

yourGateway = JPSGateway(resName=yourResName,jarPath=yourResJarPath,**JGkwargs)
```

where `resName` is the name you wish to give to your resource, `yourResJarPath` is the absolute path to the resource main jar file and `**JGkwargs` represents a dictionary of any optional argument: value pairs to pass to the [py4j JavaGateway](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.launch_gateway) class. Note that if you wish to access an already installed resource, the `jarPath` argument can be omitted as it can be looked up by the resource name.

To actually start the communication with the Java side, please run the following command:

```python
yourGateway.launchGateway(**LGkwargs)
```

where the `**LGkwargs` represents a dictionary of any optional argument: value pairs supported by the [py4j JavaGateway.launch_gateway](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.launch_gateway) method. Note that the `py4j jarpath` argument cannot be changed in this call and will be omitted from the `**LGkwargs` dictionary. The proper way to set the jarpath is via the `JPSGateway` constructor. One example of the optional `py4j JavaGateway.launch_gateway` arguments that could be useful to pass are the `redirect_stdout=stdout_file_handle` and `redirect_stderr=std_err_file_handle`. These arguments direct the stdout and stderr streams to a file, which might be useful for debugging pruposes. Here is a full example on how to do it:
```python
# The file paths are exemplary only, please change
# according to your needs
stdout_file_handle = open('D:\\stdout_file.out','w')
sterr_file_handle = open('D:\\stderr_file.out','w')

yourGateway.launchGateway(**{'redirect_stdout':stdout_file_handle,'redirect_stderr':sterr_file_handle})
```

Once your resource gateway is launched you can access any public classess and methods in a following way:

```python
KGRouter = yourGateway.gateway.jvm.uk.ac.cam.cares.jps.base.query.KGRouter
```

As can be seen, the gateway's jvm class serves as an entry point to any java classes and objects you wish to access. Simply provide the fully qualified name of the resource to access it. The `uk.ac.cam.cares.jps.base.query.KGRouter` path is for example purposes only, and depends on the java resource and object you are working with. **Please note that though accessing objects via their fully qualified names is possible, it is not recommended**. A much simpler way exists, through the java import statemets explained in the **JVM module views** section.

Please also note that via the `jvm` field you can access any default java collections and arrays as these are automatically mapped to Python collections. Here is an example from the [py4j documentaion](https://www.py4j.org/advanced_topics.html):
```python
int_class = yourGateway.gateway.jvm.int
int_array = yourGateway.gateway.new_array(int_class,2)
int_array[0] = 1
# etc..
```
Construction of java collections such as the `int_array` or `string_array` could be needed if one wishes to call a java method that requires them as arguments and the call fails with the default Python int / string lists. **This however, should be very rare** given that the auto conversion of Python objects like sequences and maps to Java Objects is enabled by default in `py4jps` by setting the `py4j JavaGateway` argument, `auto_convert`, to True.

## Resource gateways

The resource gateway class is automatically created upon the resource installation. The class name is the same as the name of the resource given during the installation. Furthermore, **each resource gateway class inherits from the `py4jps JPSGateway` class**. Therefore, **one should ideally only work with the resource classes rather than with the parent JPSGateway class**. It is also recommended to have only one gateway object in your Python application per java resource you wish to access. This can be easily achieved by instantiating and starting the resource gateway objects in a specially designed module e.g. `jpsSingletons` and then to import these objects instances to any other module that requires it.

To give a simple example, after installing the `jpsBaseLib` resource one can import and use its gateway in a following way:
```python
from py4jps.resources import jpsBaseLib

jpsBaseLibGW = jpsBaseLib(**JGkwargs)
jpsBaseLibGW.launchGatewa(**LGkwargs)
```

Do note that compared to the `JPSGateway` class the `jpsBaseLib` constructor call neither accepts the resource name nor the resource jar path as arguments. This ensures that the resource is properly encapsulated.

## JVM module views

`Py4j` allows importing Java packages so that you don't have to type the fully qualified names of the classes you want to instantiate. However, any such import statement is global in Python, e.g. the `jvm` instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). A convenience method has been added to the `py4jps` gateway class that allows to create the module views (see [py4j](https://www.py4j.org/advanced_topics.html#importing-packages-with-jvm-views) documentation for further details on module views):
```python
# create the module view
moduleViewInstance = resGateway.createModuleView()
```
where the `resGateway` is the gateway of a resource we wish to create the module view for. Once the module view is created, it can be used to import the desired java classes using the `importPackages` method, e.g:

```python
# import classes into the module view
resGateway.importPackages(moduleViewInstance,"uk.ac.cam.cares.jps.base.query.*")
# once classes are imported, they can be accesses as follows
KGRouter = moduleViewInstance.KGRouter
```
The module view should be created for each Python module in your application that requires access to the java resource, followed by any desired import statements. The name of the module view is arbitrary, though it is recommended to name it after your Python module file name and the parent resource to avoid confusion and potential name clashes.

## Example code:

Putting the above recommendations together leads to the following design pattern:

*Example project source files layout*

    .
    ├── ...                         # your other project files (README, LICENSE, etc..)
    └── src                         # your project source files
        ├── jpsSingletons.py        # your singleton module that instantiates and starts the resources gateway objects
        ├── main.py                 # your application main entry point
        ├── app_module1.py          # your application module 1
        └── app_module2.py          # your application module 2, etc..


Pleaes note that the above project layout is extremely simple e.g. typically the jpsSingletons.py file should be moved to its own module etc.. by this is not necessary for this code example.

*Example code*

`File: jpsSingletons.py`

```python
# The purpose of this module is to create and start resource
# gateway objects to be used in all of your other modules
#============================================================
from py4jps.resources import jpsBaseLib

# jpsBaseLib resource gateway
# you may also wish to pass any **JGkwargs
jpsBaseLibGW = jpsBaseLib()

# you may also wish to pass any **LGkwargs
jpsBaseLibGW.launchGateway()

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
