#!/bin/bash
# J. Bai (jb2197@cam.ac.uk), based on https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_wrapper/install_script_pip.sh provided by D. Nurkowski (danieln@cmclinnovations.com)

AUTHOR="Jiaru Bai <jb2197@cam.ac.uk>, based on https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_wrapper/install_script_pip.sh provided by Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CREATE_VENV='n'
VENV_NAME='pyderivationagent_venv'
VENV_DIR=$SPATH
DEV_INSTALL=''
PROJ_NAME='pyderivationagent'

function usage {
    echo "==============================================================================================================="
    echo $PROJECT_NAME" project installation script."
    echo
    echo "Please run the script with following options:"
    echo "---------------------------------------------------------------------------------------------------------------"
    echo " Usage:"
    echo "  -v [-n VENV_NAME -d VENV_DIR -i -e]"
    echo "  -i [-n VENV_NAME -d VENV_DIR -e]"
    echo "  -h"
    echo ""
    echo "Options"
	echo "  -v              : Create the base "$VENV_NAME" virtual environment for the package."
    echo "                    NOTE, if the same named environment already exists it will be REPLACED."
    echo "                    Use the -n VENV_NAME to change the default environment name"
    echo "  -i              : Install the project and its dependencies."
    echo "                    Use the -n VENV_NAME and -d VENV_DIR to install the package to a"
    echo "                    different environment e.g. as a dependency."
	echo "  -n VENV_NAME    : Name of the virtual environment to create and/or install the package to."
    echo "  -d VENV_DIR     : If used with the -v flag - directory to create the virtual at and install"
    echo "                    the package to."
    echo "                    If used with the -i flag - directory of the environment to install the package to."
	echo "  -e              : Enables developer mode installation."
	echo "  -h              : Print this usage message."
    echo ""
	echo "Example usage:"
    echo "./install_script.sh -v                            - this will create the base virt. env. for the project"
	echo "./install_script.sh -v -i                         - this will create the base virt. env. for the project"
    echo "                                                    and install it with all its dependencies"
	echo "./install_script.sh -v -i -e                      - this will create the base virt. env. for the project"
    echo "                                                    and install it in developer mode with all its dev-dependencies"
	echo "./install_script.sh -i                            - this will install the project and its dependencies in"
    echo "                                                    the default "$VENV_NAME" environment."
	echo "./install_script.sh -i -n my_env -d my_env_dir    - this will install the project and its dependencies in"
    echo "                                                    the my_env environment. Use it if you want to include the"
    echo "                                                    "$PROJECT_NAME" in a different environment as a dependency"
    echo "./install_script.sh -v -n my_env -i -e            - this will create virtual environment 'my_env' with all project"
	echo "                                                    dependencies and install the project in a developer mode"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
}

function create_env {
	echo "Creating virtual environment for this project"
    echo "-----------------------------------------------"
    echo
        echo "Creating "$VENV_NAME" virtual environment..."
		if [ -d "$VENV_DIR/$VENV_NAME" ]; then
			rm -r $VENV_DIR"/"$VENV_NAME
		fi
		python -m venv $VENV_DIR"/"$VENV_NAME
		if [ $? -eq 0 ]; then
			echo ""
			echo "    INFO: Virtual environment created."
			echo "-----------------------------------------"
		else
			echo ""
			echo "    ERROR: Failed to create virtual environment."
			echo "-----------------------------------------"
            read -n 1 -s -r -p "Press any key to continue"
			exit -1
		fi
		echo ""
}

function get_pip_path {
    if [[ $CREATE_VENV == 'y' ]]
	then
	    if [ -d "$VENV_DIR/$VENV_NAME/bin" ]; then
            PIPPATH=$VENV_DIR"/"$VENV_NAME/bin/pip3
		else
		    PIPPATH=$VENV_DIR"/"$VENV_NAME/Scripts/pip3
		fi
	else
        PIPPATH=pip3
	fi
}

function install_project {
	echo "Installing the project"
    echo "-----------------------------------------------"
    echo
    get_pip_path
    $PIPPATH --disable-pip-version-check install $DEV_INSTALL $SPATH
    if [[ "${DEV_INSTALL}" == "-e" ]];
    then
        $PIPPATH --disable-pip-version-check install -e $SPATH"[dev]"
    fi
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

function install_agentlogging_workaround {
    echo "Installing the agentlogging package"
    echo "-----------------------------------------------"
    echo "As PyPI does NOT allow install_requires direct"
    echo "links, so we could NOT add package agentlogging"
    echo "from 'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils'"
    echo "as dependency, therefore, in order to pass the"
    echo "run_pyderivationagent_tests() and release_to_pypi(),"
    echo " we here introduce a workaround here to install"
    echo "agentlogging to the virtual environment but NOT"
    echo "as dependency in the setup.py"
    echo "-----------------------------------------------"
    echo
    get_pip_path
    $PIPPATH --disable-pip-version-check install "git+https://github.com/cambridge-cares/TheWorldAvatar@develop#subdirectory=Agents/utils/python-utils"

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
    -h)
     usage;;
    -v) CREATE_VENV='y'; shift;;
	-n) VENV_NAME=$2; shift 2;;
	-d) VENV_DIR=$2; shift 2;;
    -i) INSTALL_PROJ='y'; shift;;
	-e) DEV_INSTALL='-e'; shift;;
    *)
	# otherwise print the usage
    usage
    ;;
esac

done

if [[ $CREATE_VENV == 'y' ]]
then
    create_env
fi
if [[ $INSTALL_PROJ == 'y' ]]
then
    install_project
    install_agentlogging_workaround
fi

echo
read -n 1 -s -r -p "Press any key to continue"

