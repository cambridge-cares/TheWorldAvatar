#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
# J. Bai (jb2197@cam.ac.uk)
#
# py4jps release script
#
AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>; Jiaru Bai <jb2197@cam.ac.uk>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
VENV_NAME='py4jps_venv'
TEST_VENV_NAME='test_venv'
PROJECT_NAME='py4jps'
TEST_PYPI="https://test.pypi.org/legacy/"
DEP_FILE='dependencies.yml'
STEP_NR=1
NEXT_VERSION=''

usage() {
    echo "==============================================================================================================="
    echo $PROJECT_NAME" project release script."
    echo
    echo "Please run the script with following options:"
    echo "---------------------------------------------------------------------------------------------------------------"
    echo " Usage:"
    echo "  -v NEXT_VERSION"
    echo "  -h"
    echo ""
    echo "Options"
	echo "  -v              : Release the $PROJECT_NAME with the following version."
	echo "  -h              : Print this usage message."
    echo ""
	echo "Example usage:"
    echo "./release_py4jps_to_pypi.sh -v 1.0.17   - release version 1.0.17"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
}

main() {
    install_packages_for_building
    bump_py4jps_version_number
    clean_and_build_jps_base_lib
    package_jps_base_lib_with_py4jps
    build_py4jps_for_release
    release_to_pypi test-pypi
    test_release test-pypi
    release_to_pypi main-pypi
    test_release main-pypi
    read -n 1 -s -r -p "Press any key to continue"
}

install_packages_for_building() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Installing wheel and twine for building/releasing $PROJECT_NAME"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    pip install wheel twine

    STEP_NR=$((STEP_NR+1))
}

bump_py4jps_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Bumping the $PROJECT_NAME version number to $NEXT_VERSION"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    sed -bi "s/version=.*,/version='$NEXT_VERSION',/" $SPATH/setup.py
    sed -bi "s/__version__ = .*/__version__ = \"$NEXT_VERSION\"/" $SPATH/$PROJECT_NAME/__init__.py

    STEP_NR=$((STEP_NR+1))
}

clean_and_build_jps_base_lib() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Building the JPS_BASE_LIB"
    echo "-------------------------------------------------------------------------"
    echo ; echo
    cd $SPATH/..
    mvn clean install -DskipTests
    if [ $? -ne 0 ]; then
        echo "Couldnt build JPS_BASE_LIB. Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    cd $SPATH/
    STEP_NR=$((STEP_NR+1))
}

clean_py4jps_repository() {
    rm -rf $SPATH/build $SPATH/dist $SPATH/.eggs $SPATH/*egg-info $SPATH/*venv
}

install_py4jps() {
    echo "-------------------------------"
    echo "Installing $PROJECT_NAME"
    echo "-------------------------------"
    clean_py4jps_repository
    sleep .5
    venv_name_local=$1
    venv_dir_local=$2

    $SPATH/install_script_pip.sh -v -i -e -n $venv_name_local -d $venv_dir_local

    if [ -d "$venv_dir_local/$venv_name_local/bin" ]; then
        PYTHON_EXEC=$venv_dir_local/$venv_name_local/bin/python
        PYTHON_EXEC_FOLDER=bin
    else
        PYTHON_EXEC=$venv_dir_local/$venv_name_local/Scripts/python
        PYTHON_EXEC_FOLDER=Scripts
    fi
}

run_py4jps_tests() {
    echo "-------------------------------"
    echo "Running the $PROJECT_NAME tests"
    echo "-------------------------------"
    echo
    echo
    echo $1
    $1 -m pytest tests/
    TEST_RESULT=$?
    if [ $TEST_RESULT -eq 0 ]; then
        echo "All tests have completed successfully"
    else
        echo "Aborting the release. Some tests have failed. Please check the pytest detailed output for more details."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
}

package_jps_base_lib_with_py4jps() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Packaging the $PROJECT_NAME project"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    install_py4jps $VENV_NAME $SPATH

    $SPATH/$VENV_NAME/$PYTHON_EXEC_FOLDER/jpsrm uninstall JpsBaseLib
    $SPATH/$VENV_NAME/$PYTHON_EXEC_FOLDER/jpsrm devinstall

    run_py4jps_tests $PYTHON_EXEC

    STEP_NR=$((STEP_NR+1))
}

build_py4jps_for_release() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Building the $PROJECT_NAME for the release"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    clean_py4jps_repository
    sleep .5
    python setup.py sdist bdist_wheel
    if [ $? -eq 0 ]; then
        echo "Build successfull. Checking the distribution artifacts."
        python -m twine check dist/*
        if [ $? -ne 0 ]; then
            echo "Problem with distribution artifacts. Aborting the release."
            read -n 1 -s -r -p "Press any key to continue"
            exit -1
        fi
    else
        echo "Building $PROJECT_NAME has failed. Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    STEP_NR=$((STEP_NR+1))
}

release_to_pypi() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Releasing the $PROJECT_NAME to $1"
    echo "-------------------------------------------------------------------------"
    echo
    echo

    read -p "Enter your username: " username
    read -s -p "Enter your password: " password
    echo
    if [ $1 = "main-pypi" ]; then
        echo "main release"
        python -m twine upload -u $username -p $password $SPATH/dist/*
    else
        echo "test release"
        python -m twine upload -u $username -p $password --repository-url $TEST_PYPI $SPATH/dist/*
    fi
    if [ $? -ne 0 ]; then
        echo "Couldnt upload artifacts to $1. Have you forgotten to increse the $PROJECT_NAME version number?"
        echo "Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    STEP_NR=$((STEP_NR+1))
}

test_release() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Testing the $PROJECT_NAME $1 release."
    echo "-------------------------------------------------------------------------"
    echo
    echo

    echo "Creating test venv."
    rm -rf $SPATH/../$TEST_VENV_NAME
    sleep .5
    python -m venv $SPATH/../$TEST_VENV_NAME

    if [ -d "$SPATH/../$TEST_VENV_NAME/bin" ]; then
        PYTHON_EXEC=$SPATH/../$TEST_VENV_NAME/bin/python
    else
        PYTHON_EXEC=$SPATH/../$TEST_VENV_NAME/Scripts/python
    fi

    echo "Waiting 2 minutes for the $1 to update its package index"
    sleep 24
    echo -ne '####                      (20%)\r'
    sleep 24
    echo -ne '########                  (40%)\r'
    sleep 24
    echo -ne '############              (60%)\r'
    sleep 24
    echo -ne '################          (80%)\r'
    sleep 24
    echo -ne '####################      (100%)\r'
    echo -ne '\n'

    $PYTHON_EXEC -m pip install --upgrade pip
    if [ $1 = "test-pypi" ]; then
        $PYTHON_EXEC -m pip install --no-cache-dir --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple $PROJECT_NAME
    else
        $PYTHON_EXEC -m pip install --no-cache-dir --upgrade $PROJECT_NAME
    fi
    $PYTHON_EXEC -m pip install pytest testcontainers

    run_py4jps_tests $PYTHON_EXEC

    echo "Removing test venv."
    rm -rf $SPATH/../$TEST_VENV_NAME

    STEP_NR=$((STEP_NR+1))
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
        -v) NEXT_VERSION=$2; shift 2;;
        *)
        # otherwise print the usage
        usage;;
    esac
done

main "$@"; exit
