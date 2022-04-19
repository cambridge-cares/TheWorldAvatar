# Description

The `MetOffice` agent is an input and output agent which queries data from the MetOffice API, also known as [DataPoint], and instantiates it according to the [OntoEMS] ontology in the [TheWorldAvatar] knowledge graph.

# Installation
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Requirements

- You need Python >3.7 to run the `MetOffice` agent. You can install Python by going to the official Python [download page]
- You also need to install a [Java Runtime Environment version >=8]

## Virtual environment setup

It is highly recommended to use a [virtual environment] for the `MetOffice` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv metoff_venv
$ metoff_venv\Scripts\activate.bat
(metoff_venv) $
```

The above commands will create and activate the virtual environment `metoff_venv` in the current directory.

## Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `MetOffice`  directly from its repository you need to first clone the [TheWorldAvatar] project. Then simply navigate to the *TheWorldAvatar\Agents\MetOfficeAgent* directory and execute the following commands:
```bash
# build and install
(metoff_venv) $ python -m pip install .
(metoff_venv) $ python -m pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"

# or build for in-place development
(metoff_venv) $ python -m pip install -e .
(metoff_venv) $ python -m pip install -r dev_requirements.txt
(metoff_venv) $ python -m pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"
```

Alternatively, use the provided `install_script_pip.sh` convenience scripts, that can create virtual environment and install the `MetOffice` in one go:
```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```
Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(metoff_venv) $ pytest tests\test_termocalc.py
```

# How to use

The `MetOffice` agent can be used as a simple command line tool or as a web agent.

## Web agent usage

In order to use the `MetOffice` as a web agent, simply start a server with the following app entry point:

`(Windows)`

```cmd
(stdc_venv) $ set FLASK_APP=metoffice\flaskapp\wsgi.py & flask run
```

## Dockerized agent


### Debugging

```
docker-compose -f "docker-compose.yml" up -d --build metoffice_agent_debug
```
attach debugger to start process
send requests, execute scheduled tasks
`launch.json`
```
        {
            "name": "Python: Remote Attach",
            "type": "python",
            "request": "attach",
            "connect": {
                "host": "127.0.0.1",
                "port": 5678
            },
            "pathMappings": [
                {
                    "localRoot": "${workspaceFolder}/metoffice/flaskapp/",
                    "remoteRoot": "/app/metoffice/flaskapp/"
                }
            ]
        }
```


### Production

docker-compose -f "docker-compose.yml" up -d --build metoffice_agent_production

# Notes on tests

Please note that some of the tests use the `testcontainers` library and, hence, require Docker to be installed. Furthermore, access to the `docker.cmclinnovations.com registry` is required from the machine the test is run on to pull docker images.  
You can request login details by emailing `support<at>cmclinnovations.com` with the subject 'Docker registry access'

# Authors #
Markus Hofmeister (mh807@cam.ac.uk), March 2022

(Parts of the agent leverage code initially developed by Daniel Nurkowski (danieln@cmclinnovations.com))


<!-- Links -->
[DataPoint]: https://www.metoffice.gov.uk/services/data/datapoint/about
[OntoEMS]: http://www.theworldavatar.com/ontology/ontoems/OntoEMS.owl
[download page]: https://www.python.org/getit/
[Java Runtime Environment version >=8]: https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot
[virtual environment]: https://docs.python.org/3/tutorial/venv.html
[TheWorldAvatar]: https://github.com/cambridge-cares/TheWorldAvatar