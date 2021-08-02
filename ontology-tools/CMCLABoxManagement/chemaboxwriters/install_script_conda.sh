#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)

AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
VENV_NAME='chemaboxwriters_venv'
PROJECT_NAME='chemaboxwriters'
DEP_FILE='dependencies.yml'
DEV_INSTALL=''
DO_NOT_PROMPT='n'

function usage {
    echo "==============================================================================================================="
    echo $PROJECT_NAME" project installation script."
    echo
    echo "Please run the script with following options:"
    echo "---------------------------------------------------------------------------------------------------------------"
    echo " Usage:"
    echo "  -v [-n VENV_NAME -i -e -s]"
    echo "  -i [-n VENV_NAME -e -s]"
    echo "  -h"
    echo ""
    echo "Options"
	echo "  -v              : Create the base "$VENV_NAME" conda virtual environment for the package."
    echo "                    NOTE, if the same named environment already exists it will be REPLACED."
    echo "                    Use the -n VENV_NAME to change the default environment name"
    echo "  -i              : Install the project and its dependencies."
    echo "                    Use the -n VENV_NAME to install the package to a different environment"
    echo "                    e.g. as a dependency."
	echo "  -n VENV_NAME    : Name of the virtual environment to create and/or install the package to."
	echo "  -e              : Enables developer mode installation."
    echo "  -s              : Silent mode, do not prompt for a user input unless necessary."
	echo "  -h              : Print this usage message."
	echo
	echo "Example usage:"
    echo "./install_script.sh -v                  - this will create the base virt. env. for the project"
	echo "./install_script.sh -v -i               - this will create the base virt. env. for the project and install it"
    echo "                                          with all its dependencies"
	echo "./install_script.sh -v -i -e            - this will create the base virt. env. for the project and install it"
    echo "                                          in developer mode with all its dev-dependencies"
	echo "./install_script.sh -i                  - this will install the project and its dependencies in the default"
    echo "                                          "$VENV_NAME" environment."
	echo "./install_script.sh -i -n my_env        - this will install the project and its dependencies in the my_env"
    echo "                                          environment. Use it if you want to include the "$PROJECT_NAME
    echo "                                          in a different environment as a dependency"
    echo "./install_script.sh -v -n my_env -i -e  - this will create virtual environment 'my_env' with all project"
	echo "                                          dependencies and install the project in a developer mode"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
}

function prompt_for_input {
    if [[ $DO_NOT_PROMPT == 'n' ]]
    then
		read -n 1 -s -r -p "Press any key to continue"
    fi
}

function check_conda {
    echo "Verifying conda installation."
    echo "-------------------------------------------------------------"
    conda_version=$(conda --version)
    if [ $? -eq 0 ]; then
        echo
        echo "INFO: Found "$conda_version
    else
        echo "ERROR: Could not find conda installation. On Windows, you must run this script from Anaconda Prompt for conda to be correctly located. Aborting installation."
        prompt_for_input
        exit -1
    fi
    echo
    echo
}

function recreate_conda_env {
    echo "Creating the "$PROJECT_NAME" base conda environment"
    echo "-------------------------------------------------------------"
    echo
    # This will recreate conda environment
    conda config --set channel_priority strict
    conda remove -n $VENV_NAME --all
    # create a new environment

    conda env create -f $SPATH/base.yml -n $VENV_NAME

    if [ $? -eq 0 ]; then
        echo
        echo "INFO: Conda environment successfully created."
    else
        echo "ERROR: Could not create conda environment."
        prompt_for_input
		exit -1
    fi
    echo
    echo
}

function install_project {
	echo "Installing the "$PROJECT_NAME" project"
    echo "-----------------------------------------------"
    echo

	install_local_dependencies
    conda env update -n $VENV_NAME -f $SPATH/$DEP_FILE

    if [ $? -eq 0 ]; then
    	echo ""
    	echo "    INFO: installation complete."
    	echo "-----------------------------------------"
    else
        echo ""
    	echo ""
    	echo "    ERROR: installation failed."
    	echo "-----------------------------------------"
		prompt_for_input
		exit -1
    fi
}

function install_local_dependencies {
	echo "Installing project local dependencies."
    echo "-----------------------------------------------"
    echo ""
	$SPATH/../../../thermo/chemutils/install_script_conda.sh -i -s -n $VENV_NAME $DEV_INSTALL
    echo ""
    echo ""
	$SPATH/../../../EntityRDFizer/install_script_conda.sh -i -s -n $VENV_NAME $DEV_INSTALL
    echo ""
    echo ""
	$SPATH/../../../thermo/CoMoCompChemParser/install_script_conda.sh -i -s -n $VENV_NAME $DEV_INSTALL

    if [ $? -eq 0 ]; then
    	echo ""
    	echo "    INFO: installation of local"
        echo "          dependencies complete."
    	echo "-----------------------------------------"
        echo ""
    else
        echo ""
    	echo ""
    	echo "    ERROR: installation failed."
    	echo "-----------------------------------------"
		prompt_for_input
		exit -1
    fi
}

# Scan command-line arguments
if [[ $# = 0 ]]
then
   usage
fi
while [[ $# > 0 ]]
do
key="$1"
case $key in
    -h) usage;;
    -v) RECREATE_VENV='y'; shift;;
	-n) VENV_NAME=$2; shift 2;;
    -i) INSTALL_PROJ='y'; shift;;
    -s) DO_NOT_PROMPT='y'; shift;;
	-e) DEP_FILE='dev-dependencies.yml'; DEV_INSTALL='-e'; shift;;
     *)
	# otherwise print the usage
    usage
    ;;
esac
done

if [[ $RECREATE_VENV == 'y' ]]
then
    check_conda
    recreate_conda_env
fi
if [[ $INSTALL_PROJ == 'y' ]]
then
    install_project
fi
echo
echo "==============================================================================================================="
echo
echo
prompt_for_input
exit
