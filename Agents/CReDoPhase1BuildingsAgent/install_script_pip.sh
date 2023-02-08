#!/bin/bash
# Initial version by D. Nurkowski (danieln@cmclinnovations.com)
echo "------------------------------------------------------"
echo "--   Python \"kingslynnagent\" installation script  --"
echo "------------------------------------------------------"
echo ""
#

SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CREATE_VENV='n'
# TODO: Set name of virtual environment
VENV_NAME='kingslynn-venv'
VENV_DIR=$SPATH
DEV_INSTALL=''
# TODO: Set Python project name (as in setup.py)
PROJ_NAME='kingslynnagent'

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

function create_venv {
	  echo "Creating virtual environment for this project"
    echo "-----------------------------------------------"
    echo
    echo "Creating "$VENV_NAME" virtual environment..."
		if [ -d "$VENV_DIR/$VENV_NAME" ]; then
			rm -r $VENV_DIR"/"$VENV_NAME
		fi
		# TODO: Potentially specify specific Python version to use
		#py -3.7 -m venv $VENV_DIR"/"$VENV_NAME
		py -m venv $VENV_DIR"/"$VENV_NAME
		if [ $? -eq 0 ]; then
			echo ""
			echo "    INFO: Virtual environment created."
			echo "-----------------------------------------"
            chmod -R 755 $VENV_DIR"/"$VENV_NAME
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
	    if [ -f "$VENV_DIR/$VENV_NAME/bin/pip3" ]; then
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
    # Activate virtual environment
    "$VENV_NAME/Scripts/activate"
    get_pip_path
    if [[ "${DEV_INSTALL}" == "-e" ]];
    then
		# Install requirements for development mode (incl. pytest etc.)
    $PIPPATH --disable-pip-version-check install -r $SPATH"/"dev_requirements.txt
		# install own Python project
	  $PIPPATH --disable-pip-version-check install -e $SPATH
	  else
		# install requirements for non-development mode only
		$PIPPATH --disable-pip-version-check install -r $SPATH"/"requirements.txt
		# install own Python project
		$PIPPATH --disable-pip-version-check install $SPATH
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
    create_venv
fi
if [[ $INSTALL_PROJ == 'y' ]]
then
    install_project
fi

echo
read -n 1 -s -r -p "Press any key to continue"
