## What is `twa`

`twa` is a Python wrapper for [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. The code is heavily based on the [py4j](https://www.py4j.org/index.html) package, which enables Python programs running in a Python interpreter to dynamically access Java objects in a Java Virtual Machine. It has a precedent python package, [`py4jps`](https://pypi.org/project/py4jps/), which is now deprecated.

## Notes to developers

The aim of `twa` is to provide Python access to the Java classes and methods in `TheWorldAvatar` project. The Python-Java communication, handled by the [py4j](https://www.py4j.org/index.html) package, happens via the local network sockets. The main interaction point between a Python and Java is provided by the [py4j.java_gateway.JavaGateway](https://www.py4j.org/py4j_java_gateway.html#javagateway) class. This class is wrapped within the `twa.JPSGateway` class for convenience. The `JPSGateway` class is the parent class for any installed java resources that one wishes to access. **Although, it is not recommended to use it directly**, the `JPSGateway` class can be useful in experimenting with different resources without installing them first, as it allows to change the resources at runtime.

However, it is important to understand that not all `TheWorldAvatar` classes/functions in Java are provided with a high-level abstraction in `twa`. Therefore, the java project documentation should also be consulted with to know which Java objects to call to perform a desired task. Additionally, not all `TheWorldAvatar` classes can be accessed. Namely, **any servlet depending classes can not be instantiated in Python without running the Apache Tomcat server first**. Since this has not been tested, it is not guaranteed that running the Apache Tomcat server would fix the problem. However, this should not be an issue for the `twa` users, given that the main purpose of the wrapper is to use the client-side `TheWorldAvatar` Java code to perform knowledge graph operations (e.g. queries and updates). Should the developers wish to develop agents (server-side code which normally developed in Java) in Python using `twa`, the native Python class `DerivationAgent` should be used.

## Authors

Jiaru Bai

Daniel Nurkowski
