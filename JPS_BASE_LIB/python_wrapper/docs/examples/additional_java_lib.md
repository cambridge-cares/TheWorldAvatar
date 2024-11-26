[`stack-clients`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-clients) is another commonly used Java library within `TheWorldAvatar`. It is therefore chosen as an example to demonstrate how to use additional Java library. The same principle can be applied to any other Java packages.

## Build and package the jar files

> **All commands tested in WSL2.**

The first step is to build the `stack-clients` package:
```sh
cd TheWorldAvatar/Deploy/stacks/dynamic/stack-clients
mvn clean install -DskipTests
```

Then copy the `stack-clients.jar` file and the `lib` folder into a temporary folder, say name `tmp_stack`:
```sh
mkdir tmp_stack
cp target/stack-clients-*.jar tmp_stack/
cp -r target/lib tmp_stack/lib
```

Finally install the stack-clients library into the `twa`:
```sh
jpsrm install StackClients tmp_stack
```

> Note that here we are not providing the `--jar` option so that `jpsrm` will use the first jar file it finds in the `tmp_stack` folder. Given that there is only one jar file in this folder, this will work just fine.

Upon successful installation, you will see below messages in the concole:
```
Info: Adding the StackClients resource...
Info: Adding StackClients resource to the registry.
Info: Fetching StackClients resource files.
Info: Installing StackClients files...
Info: Installing StackClients files complete.
Info: Saving the registry.
```

The temporary folder can now be safely removed:
> **`rm -rf` should be used with caution!!!**

```sh
rm -rf tmp_stack
```


## Instantiate and launch gateway

> Note that it is recommended to have **ONLY ONE** gateway object in your Python application per java resource you wish to access. This can be easily achieved by instantiating and starting the resource gateway objects in a single module and then to import these objects instances to any other module that requires it.

Therefore, one can instantiate the gateway for `StackClients` in the following way:

`File: stack_clients_gateway.py`

```python
from twa.resources import StackClients


stackClientsGw = StackClients()
stackClientsGw.launchGateway()
```

and importing it in any other Python modules:

`File: my_python_script.py`

```python
from stack_clients_gateway import stackClientsGw

# Create module views to relevant Stack clients
stackClientsView = stackClientsGw.createModuleView()
stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")

# Retrieve endpoint configurations from Stack clients
containerClient = stackClientsView.ContainerClient()

# other custom codes...
```
