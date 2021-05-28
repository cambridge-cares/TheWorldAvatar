#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
echo "-----------------------------------------------"
echo "--       QCParser installation script        --"
echo "-----------------------------------------------"
echo ""
#

AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CREATE_VENV='n'
VENV_NAME='qcparser_venv'
VENV_DIR=$SPATH
DEV_INSTALL=''


function usage {
    echo "==============================================================================================================="
    echo "QCParser project installation script."
    echo
    echo "Please run the script with one of the following flags set:"
    echo "--------------------------------------------------------------------------------------------------------"
	echo "  -v              : creates virtual environment for this project in the project directory"
	echo "  -n VENV_NAME    : name of the virtual environment to be created, if not provided,"
	echo "                    default "$VENV_NAME" will be used instead"
	echo "  -d VENV_DIR     : directory specifying where to create the virtual environment, if not provided,"
	echo "                    the virtual environment will be created in the root of the project directory"
	echo "  -i              : installs the project in currently active virtual environment"
	echo "  -e              : enables developer mode installation"
	echo "  -h              : print this help message"
	echo
	echo "Example usage:"
	echo "./install_parser.sh -i              - this will install the project in the currently active virtual environment"
	echo "./install_parser.sh -v -i           - this will create default virtual environment in the project root and"
	echo "                                      install the project in it"
	echo "./install_parser.sh -v -i -e        - this will create default virtual environment in the project root and "
	echo "                                      install it in a developer mode"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
}

function create_env {
	echo "Creating virtual environment for this project"
    echo "-----------------------------------------------"
    echo
        echo "Creating "$VENV_NAME" virtual environment..."
		if [ -d "$SPATH/$VENV_NAME" ]; then
			rm -r $SPATH"/"$VENV_NAME
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


function install_project {
	echo "Installing the project"
    echo "-----------------------------------------------"
    echo
    if [[ $CREATE_VENV == 'y' ]]
	then
	    if [ -d "$VENV_DIR/$VENV_NAME/bin/pip3" ]; then
            $VENV_DIR"/"$VENV_NAME/bin/pip3 --disable-pip-version-check install $DEV_INSTALL $SPATH
		else
		    $VENV_DIR"/"$VENV_NAME/Scripts/pip3 --disable-pip-version-check install $DEV_INSTALL $SPATH
		fi
	else
        pip3 --disable-pip-version-check install $DEV_INSTALL $SPATH
	fi
    if [ $? -eq 0 ]; then
    	echo ""
    	echo "    INFO: installation complete."
    	echo "-----------------------------------------"
		echo
		echo
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
	-e) DEV_INSTALL=' -e '; shift;;
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
fi

echo
read -n 1 -s -r -p "Press any key to continue"