#!/bin/bash
# L. Pascazio 
# A. Naseri
echo "-----------------------------------------------"
echo "--   python entrdfizer installation script  --"
echo "-----------------------------------------------"
echo ""
#

AUTHOR="L. Pascazio <lp521@cam.ac.uk>, A. Naseri"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CREATE_VENV='n'
VENV_NAME='pubchemagent_venv'
VENV_DIR=$SPATH
DEV_INSTALL=''
PROJ_NAME='pubchemagent'

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
    echo "Detecting suitable Python interpreter (>= 3.9)..."

    # Try python3.12 → python3.11 → python3.10 → python3.9 → fallback
    for candidate in python3.12 python3.11 python3.10 python3.9 python3 python; do
        if command -v $candidate >/dev/null 2>&1; then
            PYTHON=$candidate
            break
        fi
    done

    if [ -z "$PYTHON" ]; then
        echo ""
        echo "    ERROR: No Python interpreter found on the system."
        echo "-----------------------------------------"
        exit 1
    fi

    # Extract version number (major.minor)
    VERSION=$($PYTHON -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')

    # Check version requirement (>= 3.9)
    REQUIRED_MAJOR=3
    REQUIRED_MINOR=9

    MAJOR=$(echo $VERSION | cut -d. -f1)
    MINOR=$(echo $VERSION | cut -d. -f2)

    if [ "$MAJOR" -lt "$REQUIRED_MAJOR" ] || { [ "$MAJOR" -eq "$REQUIRED_MAJOR" ] && [ "$MINOR" -lt "$REQUIRED_MINOR" ]; }; then
        echo ""
        echo "    ERROR: Python $VERSION is too old."
        echo "           Python >= 3.9 is required for this project."
        echo "-----------------------------------------"
        exit 1
    fi

    echo "    INFO: Using Python $VERSION ($PYTHON)"
    echo ""

    echo "Creating $VENV_NAME virtual environment..."

    # Remove previous virtual environment if it exists
    if [ -d "$VENV_DIR/$VENV_NAME" ]; then
        rm -r "$VENV_DIR/$VENV_NAME"
    fi

    # Create the virtual environment
    $PYTHON -m venv "$VENV_DIR/$VENV_NAME"
    if [ $? -eq 0 ]; then
        echo ""
        echo "    INFO: Virtual environment created successfully."
        echo "-----------------------------------------"
    else
        echo ""
        echo "    ERROR: Failed to create virtual environment."
        echo "-----------------------------------------"
        exit 1
    fi

    echo ""
}

function get_pip_path {

    # *** FIX: Correct detection of pip3 path ***
    if [[ $CREATE_VENV == 'y' ]]; then
        
        if [ -f "$VENV_DIR/$VENV_NAME/bin/pip3" ]; then
            # Linux/macOS venv path
            PIPPATH="$VENV_DIR/$VENV_NAME/bin/pip3"
        elif [ -f "$VENV_DIR/$VENV_NAME/Scripts/pip3.exe" ]; then
            # Windows venv path
            PIPPATH="$VENV_DIR/$VENV_NAME/Scripts/pip3.exe"
        elif [ -f "$VENV_DIR/$VENV_NAME/Scripts/pip3" ]; then
            # Some Windows variants
            PIPPATH="$VENV_DIR/$VENV_NAME/Scripts/pip3"
        else
            echo "ERROR: Could not locate pip3 in the virtual environment."
            echo "Checked:"
            echo "  $VENV_DIR/$VENV_NAME/bin/pip3"
            echo "  $VENV_DIR/$VENV_NAME/Scripts/pip3"
            exit 1
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

    # Always install local source in editable mode
    $PIPPATH --disable-pip-version-check install -e "$SPATH"

    if [[ "${DEV_INSTALL}" == "-e" ]]; then
        $PIPPATH --disable-pip-version-check install -r "$SPATH/dev_requirements.txt"
    fi

    if [ $? -eq 0 ]; then
        echo ""
        echo "    INFO: installation complete."
        echo "-----------------------------------------"
    else
        echo ""
        echo "    ERROR: installation failed."
        echo "-----------------------------------------"
        exit -1
    fi
}

# Parse arguments
if [[ $# = 0 ]]; then
   usage
fi

while [[ $# > 0 ]]; do
key="$1"
case $key in
    -h) usage;;
    -v) CREATE_VENV='y'; shift;;
	-n) VENV_NAME=$2; shift 2;;
	-d) VENV_DIR=$2; shift 2;;
    -i) INSTALL_PROJ='y'; shift;;
	-e) DEV_INSTALL='-e'; shift;;
    *) usage;;
esac
done

if [[ $CREATE_VENV == 'y' ]]; then
    create_env
fi
if [[ $INSTALL_PROJ == 'y' ]]; then
    install_project
fi

echo
read -n 1 -s -r -p "Press any key to continue"
