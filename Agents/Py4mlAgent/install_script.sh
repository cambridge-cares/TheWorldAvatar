#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)

AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )/"
DATA_LOCAL="./data/processed"
VENV_NAME='py4ml_venv'
GET_RESOURCES='n'
DEV_INSTALL=''
GPU_VERSION=''

function usage {
    echo "==============================================================================================================="
    echo "py4ml project installation script."
    echo
    echo "Please run the script with one of the following flags set:"
    echo "---------------------------------------------------------------------------------------------------------------"
	echo "  -v              : creates conda virtual environment for this project in the project directory and"
    echo "                    installs all its dependencies. Note, if the same named environment already exists"
    echo "                    it will be removed and created again"
	echo "  -n VENV_NAME    : name of the virtual environment to be created, if not provided,"
	echo "                    default "$VENV_NAME" will be used instead"
    echo "  -g              : install cuda-enabled version allowing to run training on gpu"
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

    if [[ $GPU_VERSION == 'y' ]]
	then
        echo "pytorch gpu version selected"
        conda env create -f environment_gpu.yml -n $VENV_NAME
    else
        echo "pytorch cpu version selected"
        conda env create -f environment_cpu.yml -n $VENV_NAME
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
    	echo "    INFO: Installation complete."
    	echo "-----------------------------------------"
    else
        echo ""
    	echo ""
    	echo "    ERROR: Install failed."
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
    -g) GPU_VERSION='y'; shift;;
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
exit
