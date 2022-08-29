# The Stack Manager

In the commands below placeholders are shown as `<STACK NAME>`, you will need to substitute in the required value when running the command.

## Prerequisites

### Hardware
* A total RAM size of 32GB is recommended for smooth execution, particualry in Microsoft Windows.

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

3. Create two files called `postgis_password` and `geoserver_password` in the `stack-manager/inputs/secrets/` directory. Populate the files with the intended passwords for postgis and geoserver, respectively.

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
    * The Geoserver GUI should be available at http://localhost:3838/geoserver/. Log in using the username `admin` and the password specified in the `geoserver_pasword` file.
    * The Adminer (PostgreSQL GUI) at http://localhost:3838/adminer/ui/?username=postgres&pgsql=. Enter `<STACK NAME>-postgis:5432` as the `Server` and the value from the `postgis_pasword` file as the `Password`. The `Database` slot can be left blank if you don't know what it should be.
    * The Ontop GUI should be available at http://localhost:3838/ontop/ui.
    * The Blazegraph Workbench should be available at http://localhost:3838/blazegraph/ui.
## Debugging the Stack Manager in VSCode

1. Add the following entry into top level node the JSON file `stack-manager/.vscode/settings.json`, creating the file if it doesn't exist.
    ```json
    "debug.port": "<DEBUG PORT>"
    ```
    A value around `5005` for `<DEBUG PORT>` should be appropriate.

2. In the `Run and Debug` side panel of VSCode run the `Debug (stack-manager)` configuration.

## Developing the Stack Manager in VSCode

You will need permission to push to the CMCL package repository to be able to build the stack-manager project

1. Follow the instuctions in step 1. of [Debugging the Stack Manager in VSCode](#debugging-the-stack-manager-in-vscode)

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