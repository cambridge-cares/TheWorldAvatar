#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)

AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )/"
VENV_NAME='compchemparser_venv'
PROJECT_NAME='compchemparser'
DEV_INSTALL=''

function usage {
    echo "==============================================================================================================="
    echo $PROJECT_NAME" project installation script."
    echo
    echo "Please run the script with one of the following flags set:"
    echo "---------------------------------------------------------------------------------------------------------------"
	echo "  -v              : creates conda virtual environment for this project in the project directory and"
    echo "                    installs all its dependencies. Note, if the same named environment already exists"
    echo "                    it will be removed and created again"
	echo "  -n VENV_NAME    : name of the virtual environment to be created, if not provided,"
	echo "                    default "$VENV_NAME" will be used instead"
	echo "  -i              : installs the project in the currently active virtual environment"
	echo "  -e              : enables developer mode installation"
	echo "  -h              : print this help message"
	echo
	echo "Example usage:"
    echo "./install_script.sh -v                  - this will create default virt. env. with all project dependencies"
	echo "./install_script.sh -i                  - this will install the project in the currently active virt. env."
	echo "./install_script.sh -v -i               - this will create default virt. env. with all project dependencies"
    echo "                                          and install the project"
    echo "./install_script.sh -v -n my_env -i -e  - this will create virtual environment 'my_env' with all project"
	echo "                                          dependencies and install the project in a developer mode"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
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
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    echo
    echo
}

function recreate_conda_env {
    echo "Creating conda environment and installing the package."
    echo "-------------------------------------------------------------"
    echo
    # This will recreate conda environment
    conda config --set channel_priority strict
    conda remove -n $VENV_NAME --all
    # create a new environment

    conda env create -f environment.yml -n $VENV_NAME

    if [ $? -eq 0 ]; then
        echo
        echo "INFO: Conda environment successfully created."
    else
        echo "ERROR: Could not create conda environment."
        read -n 1 -s -r -p "Press any key to continue"
		exit -1
    fi
    echo
    echo
}

function install_project {
	echo "Installing the project"
    echo "-----------------------------------------------"
    echo
    if [[ $RECREATE_VENV == 'y' ]]
	then
        eval "$(conda shell.bash hook)"
        conda activate $VENV_NAME
    fi
    pip3 --disable-pip-version-check install $DEV_INSTALL $SPATH

    if [ $? -eq 0 ]; then
    	echo ""
    	echo "    INFO: installation complete."
    	echo "-----------------------------------------"
    else
        echo ""
    	echo ""
    	echo "    ERROR: installation failed."
    	echo "-----------------------------------------"
		read -n 1 -s -r -p "Press any key to continue"
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
	-e) DEV_INSTALL=' -e '; shift;;
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
read -n 1 -s -r -p "Press any key to continue"
exit
