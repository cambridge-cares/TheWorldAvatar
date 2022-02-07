# Experiment Setup (ExpSetup) Agent
The folder contains the source, resource, and Docker setup files for the ExpSetup Agent, based on a template provide as `TheWorldAvatar/Deploy/examples/python_agent`. 

## Purpose
under development...

## Building the Docker image
Requirements:

* The Python code of this Agent requires the py4jps library, this is currently downloaded from the external PyPi website. However, a specific version of `JpsBaseLib` is used, built from the `JPS_BASE_LIB` in branch https://github.com/cambridge-cares/TheWorldAvatar/tree/135-dev-expand-derivation-framework/JPS_BASE_LIB/ to use the most updated `uk.ac.cam.cares.jps.base.derivation.DerivationClient` class. Given that (1) you are at correct branch (`135-dev-expand-derivation-framework`), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```
    cd TheWorldAvatar/JPS_BASE_LIB
    mvn clean install -DskipTests
    ```
    Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them in the `TheWorldAvatar/Agents/DoEAgent/`. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:
    ```
    # Re-install the version of JPS_BASE_LIB that is been developing
    # (sinse the newly added code is not in the release version of py4jps)
    # TO BE REMOVED WHEN MERGE TO DEVELOP
    RUN jpsrm uninstall JpsBaseLib
    RUN mkdir /jpstemp
    COPY jps-base-lib.jar ./jpstemp/jps-base-lib.jar
    COPY lib ./jpstemp/lib
    RUN jpsrm install JpsBaseLib ./jpstemp/
    ``` 
    Once the changes of the DerivationClient class are merged back to develop branch and wrapped in the latest `py4jps` release, these lines in the Dockerfile can be commented out when building the Docker Image.
* The knowledge graph endpoints used by this agent are specified using `XXXX` and `XXXX` within the `TheWorldAvatar/Agents/ExpSetupAgent/XXXX` file. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build using the following commands:
```
cd TheWorldAvatar/Agents/ExpSetupAgent/
docker-compose -f "docker-compose.yml" up -d --build
```
Or, simply right click `docker-compose.yml` file and select `Compose Up` option in Visual Studio Code.

## How to use it
under development...
