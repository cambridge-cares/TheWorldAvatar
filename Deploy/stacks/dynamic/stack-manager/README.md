# The Stack Manager

In the commands below placeholders are shown as `<STACK NAME>`, you will need to substitute in the required value when running the command.

## Prerequisites

### Hardware
* A total RAM size of 32GB is recommended for smooth execution, particularly in Microsoft Windows.

### Software
* Building and running a stack has been tested in Microsoft Windows and to some degree Linux, it has not been tested within a MacOS environment.
* Install [Git](https://git-scm.com/downloads).
* Install [Docker Desktop, or the Docker Engine and the Docker Compose plugin](https://docs.docker.com/engine/install).
* Preferably also install [VSCode](https://code.visualstudio.com/Download), required for development.
#### For development
* Install a [Java 11+ SDK](https://adoptium.net).
* Optionally, install [Python](https://www.python.org/downloads).
### Accounts
* A [GitHub account](https://github.com), with an appropriate `read:packages` (or `write:packages` if developing) [access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
* Pulling some images requires access to the Docker registry at CMCL. In case you don't have credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page.

## Spinning up a Stack

To spin up the stack (with default settings) please follow the instructions below:

1. If you haven't already, test your access to the CMCL Docker registry, simply run 
    ```console
    docker login docker.cmclinnovations.com
    ```
    If you are not already logged in then, when prompted, enter the username and password you were given.

2. Open the Workspace in the `Deploy/stacks/dynamic` directory in VSCode (or go to the `stack-manager` subdirectory within it in a `bash` terminal).

3. Create two files called `postgis_password` and `geoserver_password` in the `stack-manager/inputs/secrets/` directory. Populate the files with the intended passwords for PostGIS and GeoServer, respectively.
    It is also possible to add a `blazegraph_password` file to initialise the Blazegraph container with authentication enabled but this is currently incompatible with most agents, a future update to the `stack-client` library will help resolve this issue.

4. From a terminal in the `stack-manager` directory, start the `stack-manager` container by running the following:
    ```console
    ./stack.sh start <STACK NAME>
    ```
    This will pull the required Docker images and start the core stack containers.
    This should bring up 7 containers, i.e. gdal, ontop, adminer, postgis, blazegraph, nginx, and geoserver.
    In case not all containers start up successfully, try running the command again or check the logs for the `stack-manager` container.
5. Accessing the GUI webpages for the containers:
    * The default exposed port number exposed by Docker is `3838`. To check the exposed port number, run
        ```console
        docker service ls --filter name=<STACK NAME>-nginx
        ```
    * The Geoserver GUI should be available at http://localhost:3838/geoserver/. Log in using the username `admin` and the password specified in the `geoserver_password` file.
    * The Adminer (PostgreSQL GUI) at http://localhost:3838/adminer/ui/?username=postgres&pgsql=. Enter `<STACK NAME>-postgis:5432` as the `Server` and the value from the `postgis_password` file as the `Password`. The `Database` slot can be left blank if you don't know what it should be.
    * The Ontop GUI should be available at http://localhost:3838/ontop/ui.
    * The Blazegraph Workbench should be available at http://localhost:3838/blazegraph/ui.

## Adding custom containers

It is possible to spin up other containers in the stack using the stack-manager.
This is particularly useful for adding agents into a stack.

The stack-manager will handle creating the containers so there is no need to create the containers using `docker` or `docker compose` before running the stack-manager.

If the configuration file for a container is present when a stack is initially spun up then it will be added then.
To add a container after a stack has been spun up just add the configuration file and run the stack-manager again, the previously started containers will be unaffected.

> :warning: **Warning:** The stack-manager does not attempt to build a container's image so all images need to be built prior to running the stack-manager.

### Configuration Files

To do this add a `.json` file for each container into the [stack-manager/inputs/config](./inputs/config/) directory.
An example of the structure of this file, the one for the Ontop container, is as follows:
```json
{
    "ServiceSpec": {
        "Name": "adminer",
        "TaskTemplate": {
            "ContainerSpec": {
                "Image": "adminer:latest"
            }
        }
    },
    "endpoints": {
        "ui": {
            "url": "http://localhost:8080",
            "externalPath": "/adminer/ui"
        }
    }
}
```

The three top-level nodes are:
* `"type"`(not used in the example above): This is used to run container specific Java code when the container is started and should be ignored for user-specified containers.
* `"ServiceSpec"`: This is based on the Docker API container creation request format documented [here]("ServiceSpec").
  To specification of `"Configs"` and `"Secrets"` has been simplified so that only the name is required.
* `"endpoints"`: This is where mappings between the internal URLs and the externally accessible paths can be specified.
  The internal URL should be the one you would use if you were logged into the container and the external path is appended to `http://localhost:3838`

Other, more complex, examples of configuration files can be seen in the stack-manager's [resources directory](./src/main/resources/com/cmclinnovations/stack/services/defaults/).

### Benefits

Spinning a container up via the stack-manager provides the following benefits:
* The container is added to the stack's Docker network, this allows the agent to connect to the other stack containers using their internal URLs.
* The URLs, usernames and passwords of other containers in the stack can be retrieved using the `ContainerClient::readEndpointConfig` method at runtime, rather than having to provide them through environment variables or `.properties` files.
* Allows the classes and methods available through the stack-clients library to be used to add new data (particularly geospatial data) into the stack in a clean an consistent way.

## Debugging the Stack Manager in VSCode

1. Add the following entry into top level node the JSON file `stack-manager/.vscode/settings.json`, creating the file if it doesn't exist.
    ```json
    "debug.port": "<DEBUG PORT>"
    ```
    A value around `5005` for `<DEBUG PORT>` should be appropriate.

2. In the `Run and Debug` side panel of VSCode run the `Debug (stack-manager)` configuration.

## Developing the Stack Manager in VSCode

You will need permission to push to the CMCL package repository to be able to build the stack-manager project

1. Follow the instructions in step 1. of [Debugging the Stack Manager in VSCode](#debugging-the-stack-manager-in-vscode)

2. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-manager/docker/credentials` directory. Populate the files with your GitHub username and access token (with scope to write packages), respectively.

3. In the `Run and Debug` side panel of VSCode run the `Build and Debug (stack-manager)` configuration.

## Further remarks

* In case any of the endpoints is not resolvable after spinning up the stack, try exploring whether the specified ports might already be assigned to other program.

* To remove an Docker Swarm service (e.g. geoserver), run
    ```console
    docker service rm <STACK NAME>-<SERVICE NAME>
    ```

* To remove a single Docker Swarm stack, run
    ```console
    docker stack rm <STACK NAME>
    ```

* To (permanently) remove all Docker Swarm services, run
    ```console
    docker swarm leave --force
    ```

<!-- Links -->
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry