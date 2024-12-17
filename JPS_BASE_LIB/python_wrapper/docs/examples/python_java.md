## Supply custom arguments when launching Java gateway

The code below shows an example `twa.JPSGateway.launchGateway` method call with custom arguments `redirect_stdout` and `redirect_stderr` that might be useful for debugging purposes (both are optional `py4j.java_gateway.JavaGateway.launch_gateway` arguments):

```python
# The file paths are exemplary only, please change
# according to your needs
stdout_file_handle = open('D:\\stdout_file.out','w')
sterr_file_handle = open('D:\\stderr_file.out','w')

yourGateway.launchGateway(**{'redirect_stdout':stdout_file_handle, 'redirect_stderr':sterr_file_handle})
```


## JVM module views

`Py4j` allows importing Java packages so that you do not have to type the fully qualified names of the classes you want to access. However, any such import statement is global in Python, e.g., the `JVM` instance can be shared across multiple Python modules. Therefore, the recommended way is to use one Java Virtual Machine View (JVMView) per Python module (file). 

Taking the `JpsBaseLib` as an example, follow the below example to create a module view (see [py4j](https://www.py4j.org/advanced_topics.html#importing-packages-with-jvm-views) documentation for further details on module views):

> Note that it is recommended to have **ONLY ONE** gateway object in your Python application per java resource you wish to access. As the `jpsBaseLibGW` is already instantiated and started in `twa.kg_operations.gateway`, one can import it to any other module that requires it.

```python
# Import gateway object of JpsBaseLib
from twa.kg_operations.gateway import jpsBaseLibGW
# Create the module view
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
```

> For any other given resources, the same principle can be applied:
> ```python
> moduleViewInstance = resGateway.createModuleView()
> ```
> where the `resGateway` is the gateway of a resource we wish to create the module view for.

It is recommended to create the module view for each Python module in your application that requires access to the java resource, followed by any desired import statements (see [Accessing Java classes and methods in Python](#accessing-java-classes-and-methods-in-python)). The name of the module view is arbitrary, though it is recommended to name it after your parent resource to avoid confusion.

## Accessing Java classes and methods in Python

Once your resource gateway is launched you can access any **public** classes and methods. Here is an example on how to access the `RemoteStoreClient` class:

> NOTE that this is **NOT RECOMMENDED**. For the recommended method please see below.

```python
RemoteStoreClient = jpsBaseLibGW.gateway.jvm.uk.ac.cam.cares.jps.base.query.RemoteStoreClient
```

As can be seen, the gateway's JVM class serves as an entry point to any java classes and objects you wish to access. Simply provide the fully qualified name of the resource to access it. The `uk.ac.cam.cares.jps.base.query.StoreRouter` path is for example purposes only. **Please note that though accessing objects via their fully qualified names is possible, it is NOT RECOMMENDED**. A much simpler way exists, through the java import statements explained in the [JVM module views](#jvm-module-views), i.e. once can create the module view and use it to import the desired java classes using the `importPackages` method, e.g:

> NOTE that any such import will be **global**, i.e. it will be carried everywhere the gateway object is imported.

```python
# Import classes into the module view
jpsBaseLibGW.importPackages(jpsBaseLib_view, "uk.ac.cam.cares.jps.base.query.*")

# Once classes are imported, they can be accesses as follows
RemoteStoreClient = jpsBaseLib_view.RemoteStoreClient
```

It is also important to understand the difference in accessing the **java static and non-static methods**. The former can be done without the object instantiation, e.g.

```python
# jpsBaseLibGW.gateway.jvm.uk.ac.cam.cares.jps.base.query.StoreRouter
# Same as:
StoreRouter = jpsBaseLib_view.StoreRouter
# static method `getStoreClient` can be called without the `StoreRouter` instantiation
StoreRouter.getStoreClient('http://kb/ontokin', True, False)
```

The non-static methods can only be accessed once the object is instantiated, e.g

```python
# Create a FileUtil instance to access its non static getDirectoryFiles method
FileUtil = jpsBaseLib_view.FileUtil()
# Call the non-static getDirectoryFiles method
output = FileUtil.getDirectoryFiles(method_arguments)
```


## Python-Java objects translation

It is important to understand that calling the Java methods from Python requires a bit of attention to the type of arguments on the Java side. The simplest case is when the Java method arguments are of any primitive type (e.g. int, float, String, etc..). Then on the Python side, it is enough to pass Python primitives (e.g 5 - int, 3.4 - float, 'a_string', etc..) without even declaring their types. Primitives are automatically converted between Python and Java. Another case is when the Java method arguments are of any collections type (e.g., Array, java.util.List, java.util.Set, etc..). If the `py4j.java_gateway.JavaGateway` option `auto_convert` is set to True (which is the case by default in `twa.JPSGateway`), then the Python - Java collections objects translation happens automatically. Please see the [py4j documentation](https://www.py4j.org/advanced_topics.html#accessing-java-collections-and-arrays-from-python) for a detailed explanation of what is converted into what. Here are some code examples:
```python
# Example of calling a Java aMethod1 with an int argument
moduleViewInstance.aMethod1(5)
# Example of calling a Java aMethod2 with a float argument
moduleViewInstance.aMethod2(3.4)
# Example of calling a Java aMethod3 with a string argument
moduleViewInstance.aMethod3('a_string')
# Example of calling a Java aMethod4 with a list of strings argument
# This only works because the auto_convert option is enabled by default in twa
moduleViewInstance.aMethod4(['a_string'])
```

However, there could be cases where a Java object of a specific type needs to be created for the method call. As an example, the `FileUtil` class in the `JpsBaseLib` resource has a method called `getDirectoryFiles`. The method simply returns a list of files in a directory that matches the provided file extensions. Here is its Java interface:
```java
public List<File> getDirectoryFiles(File folder, List<String> fileExtensions)
```

As can be seen, the first argument is of type `File`, and the second is of type `List<String>`. Additionally, the method is not static so the `FileUtil` object needs to be instantiated first to be able to access this method. This requires creating a Java `File` object on the Python side so that the method can be called with the correct arguments. Now it is important to understand that the launched resource gateway allows you to not only access the classes and methods in the resources we packaged, but also any default Java classes and methods. This can be done in two ways: either via the `jpsBaseLibGW.gateway.jvm` field (not recommended) or via the created resource module view instance `jpsBaseLib_view` (recommended). Here is an example:

```python
# Import gateway object of JpsBaseLib
from twa.kg_operations.gateway import jpsBaseLibGW
# Create the module view
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
# Import required Java packages into the created JVM view
jpsBaseLibGW.importPackages(jpsBaseLibGW_view,'uk.ac.cam.cares.jps.base.util.*')

# Create a FileUtil instance to access its non-static methods
# Compare it with the StoreRouter example, where there was no need to add
# () brackets to the router, so we were only retrieving the StoreRouter class
FileUtil = jpsBaseLibGW_view.FileUtil()

# Create the required `File folder` argument to be passed to the `getDirectoryFiles` method

# Option 1 - NOT RECOMMENDED
# not recommended use of a gateway.jvm class to access the default java classes and methods
# here we are instantiating the java.io.File class
javaFolder = jpsBaseLibGW.gateway.jvm.java.io.File('D:\\my_dir')

# Option 2 - RECOMMENDED
# recommended use of JVM view to access default java classes and methods
javaFolder = jpsBaseLibGW_view.java.io.File('D:\\my_dir')

# Call the getDirectoryFiles
# Note that the `List<String> fileExtensions` argument can be passed
# as a simple Python list of strings given the enabled auto conversion
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
# This creates a Python List which stores a javaFileObj inside
PythonList = [javaFileObj]

# or

# This creates a javaArray which stores the javaFileObj inside
# this example, however, is not needed given the auto conversion
# so, one can simply pass the PythonList created above to the method
javaArray = jpsBaseLibGW_view.java.util.ArrayList()
javaArray.append(javaFileObj)
```
