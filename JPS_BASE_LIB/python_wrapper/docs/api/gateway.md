The purpose of this module is to create and start Java resource gateway objects to be used in other modules and scripts.

## Instantiation of gateway object
To begin with, the resource gateway object for `JpsBaseLib`, which allows access to `JPS_BASE_LIB`, needs to be instantiated:

```python
# To avoid unnecessary logging information from py4j package, set logger level before
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

from twa.resources import JpsBaseLib
jpsBaseLibGW = JpsBaseLib()
```

## Start the gateway object

Below method starts the communication with the Java side:

```python
jpsBaseLibGW.launchGateway()

# Alternatively, one can supply args to the launchGateway() method:
# jpsBaseLibGW.launchGateway(**LGkwargs)
```

where the `**LGkwargs` argument represents a dictionary of any optional `argument: value` pairs one wishes to pass to the [py4j launch_gateway](https://www.py4j.org/py4j_java_gateway.html#py4j.java_gateway.launch_gateway) method. Please refer to the method documentation for the description of all the possible arguments. The most important and useful settings are set by default in the `twa.JPSGateway.launchGateway` method so a user hardly ever need to pass any arguments in that call. If required, however, the `twa.JPSGateway.launchGateway` method defaults can be overwritten by simply passing their new values.

> NOTE that compared to the `twa.JPSGateway` class, the `JpsBaseLib` constructor call neither accepts the resource name nor the resource jar path as arguments. This ensures that the resource is properly encapsulated.
