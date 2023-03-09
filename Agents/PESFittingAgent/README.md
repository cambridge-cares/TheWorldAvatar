# Description #

This project has been abandoned. As it stands, it remains incomplete and only partially functional.

## Installation ##

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

## Requirements ##

- You need Python >3.5 to run the `pesfit`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version >=8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

## Virtual environment setup ##

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `pesfit` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -m venv pesfit_venv
$ pesfit_venv\Scripts\activate.bat
(pesfit_venv) $
```

`(Linux)`

```sh
$ python3 -m venv pesfit_venv
$ source pesfit_venv\bin\activate
(pesfit_venv) $
```

The above commands will create and activate the virtual environment `pesfit_venv` in the current directory.

## Installation from the version-controlled source (for developers) ##

This type of installation is only for the developers. To install `pesfit` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\Agents\PESFittingAgent* directory and execute the following commands:

```bash
# build and install
(pesfit_venv) $ python -m pip install .

# or build for in-place development
(pesfit_venv) $ python -m  pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` or `install_script_conde.sh` convenience scripts, that can create virtual environment and install the `pesfit` in one go:

```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```

Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(pesfit_venv) $ pytest tests\test_pesfit.py
```

## How to use ##

The pesfit agent requires DL_FIELD, DL_POLY (version 5 with evb module) and MoDS installed on the hpc platform to run.
The pesfit agent can be used as a simple commnad line tool.

## command line usage

The pesfit agent can be run from the command line via the `pesfit` command which accepts the following options:

```bash
Usage:
    pesfit  (--opesIRI=<ONTO_PES_SCAN_IRI>)
            (--conf-file=<CONF-FILE-NAME-OR-PATH>)

Options: 
--opesIRI=<ONTO_PES_SCAN_IRI>                   OntoPESScan IRI
--conf-file=<CONF-FILE-NAME-OR-PATH>            configuration file name (if in the working dir) or path (if not in the working dir)
```

Example usage: 

```bash
(pesfit_venv) $ pesfit --opesIRI http://www.theworldavatar.com/kb/ontopesscan/PotentialEnergySurfaceScan_b6d609c6-fe80-4bb8-aaf7-d4675ffd2638 \ --conf-file=configuartion.json
```

Configuration file:

The configuration file is a json file. An example of configuration file is in the pesfit\resources folder.

```
slurm and hpc server properties:
"hpc.server.login.user.name"        user name on the hpc server
"hpc.server.login.user.password"    password to login on the hpc server
"hpc.address"                       hpc server address        

third-party software paths on hpc:
"dl_field.executable.path"  full path to DL_FIELD executable on the hpc server 
"dl_field_lib.folder.path"  full path to DL_FIELD executable on the hpc server 
"dl_poly.executable.path"   full path to DL_POLY executable on the hpc server (version required: dl_poly version 5 with evb)
"MoDS.executable.path":     full path to MoDS executable on the hpc server

force field properties:
"classic.force.field"   Classic fore field in DL_FIELD. Example "opls2005"
"tabulated_potential"   Tabulated potential for vdw. At the moment it supports only "isoPAHAP"

DO NOT CHANGE:
"agent.class":"pesfit"
"agent.completed.job.space.prefix":"CompletedJobs",
"agent.failed.job.space.prefix":"FailedJobs",
"input.file.name":"input",
"input.file.extension":".zip",
"output.file.name":"output",
"output.file.extension":".zip",
"json.file.extension":".json",
"json.input.file.name":"input",
"slurm.script.file.name":"Slurm.sh",
"agent.initial.delay.to.start": 1,
"agent.periodic.action.interval": 10,
"max.number.of.hpc.jobs": 5
```

## web agent usage

To be implemented


## Authors ##

Laura Pascazio (lp521@lp521.cam.ac.uk), February 2022