####################################
##          GasGridAgent          ##
####################################

Required tools
===============

* Python (min 3.5) is required to use py4jps Python wrapper
  for more details, see: https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/python_wrapper or
                         https://pypi.org/project/py4jps/
						 
* Further requirements listed in requirements.txt files
	- dev_requirements.txt required for development and testing
	- requirements.txt required for deployment
						 

Set-up (in development mode)
============================

1) Install package by running (in cmd):
  $ install_script_pip.sh -v -i -e
  
  This installs all requirements from requirements.txt and the Python package itself
  
2) Activate virtual environment
  $ gasgridagent-venv\Scripts\activate.bat
  
3) If requiring a version of the JPS Base Lib that is not provided by the deployed py4jps lib: 
    Update jps-base-lib (to version including TimeSeriesClient) using in-built py4jps resource package manager
      3.1) Build latest JPS_BASE_LIB locally
      3.2) Copy jps-base-lib.jar and entire lib folder from JPS_BASE_LIB/target into a temporary folder tmp_folder somewhere
      3.3) Run in cmd:
		    $ jpsrm uninstall JpsBaseLib
		    $ jpsrm install JpsBaseLib <path_to_the_tmp_folder>


Run all tests (only available if set up in development mode)
============================================================

1) Activate virtual environment
  $ gasgridagent-venv\Scripts\activate.bat
  
2) Call pytest to run all tests (from package root directory)
  $ pytest