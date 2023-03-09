#!/bin/bash
# J. Bai (jb2197@cam.ac.uk), based on https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_wrapper/release_py4jps_to_pypi.sh provided by D. Nurkowski (danieln@cmclinnovations.com)
#
# pyderivationagent release script
#
AUTHOR="Jiaru Bai <jb2197@cam.ac.uk>, based on https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_wrapper/release_py4jps_to_pypi.sh provided by Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
VENV_NAME='pyderivationagent_venv'
TEST_VENV_NAME='test_venv'
PROJECT_NAME='pyderivationagent'
TEST_PYPI="https://test.pypi.org/legacy/"
STEP_NR=1
NEXT_VERSION=''
DEV_VERSION=''

usage() {
    echo "==============================================================================================================="
    echo $PROJECT_NAME" project release script."
    echo
    echo "Please run the script with following options:"
    echo "---------------------------------------------------------------------------------------------------------------"
    echo " Usage:"
    echo "  -d DEV_VERSION"
    echo "  -v NEXT_VERSION"
    echo "  -h"
    echo ""
    echo "Options"
    echo "  -d              : Release the $PROJECT_NAME to the TestPyPI repository (default)."
    echo "  -v              : Release the $PROJECT_NAME with the following version."
    echo "  -h              : Print this usage message."
    echo ""
    echo "Example usage:"
    echo "./release_pyderivationagent_to_pypi.sh -v 0.0.5   - release version 0.0.5"
    echo "./release_pyderivationagent_to_pypi.sh -d 0.0.5a  - release version 0.0.5a to the TestPyPI repository"
    echo "==============================================================================================================="
    read -n 1 -s -r -p "Press any key to continue"
    exit
}

main() {
    check_dev_version_number
    install_packages_for_building
    bump_pyderivationagent_version_number
    install_pyderivationagent_and_test
    build_pyderivationagent_for_release
    release_to_pypi test-pypi
    test_release test-pypi
    release_to_pypi main-pypi
    test_release main-pypi
    read -n 1 -s -r -p "Press any key to continue"
}

check_dev_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Checking the $PROJECT_NAME stable release version number"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    if [[ "$NEXT_VERSION" =~ ^[0-9]*\.[0-9]*\.[0-9]*$ ]]
    then
        echo "Version number is correct. Proceeding with the stable release."
    else
        echo "The specified $PROJECT_NAME stable version number is $NEXT_VERSION, which is NOT correct. It should be something similar to 1.0.0. Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi

    STEP_NR=$((STEP_NR+1))
}

install_packages_for_building() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Installing wheel and twine for building/releasing $PROJECT_NAME"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    pip install wheel twine

    STEP_NR=$((STEP_NR+1))
}

bump_pyderivationagent_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Bumping the $PROJECT_NAME version number to $NEXT_VERSION"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    sed -bi "s/version=.*,/version='$NEXT_VERSION',/" $SPATH/setup.py
    sed -bi "s/__version__ = .*/__version__ = \"$NEXT_VERSION\"/" $SPATH/$PROJECT_NAME/__init__.py

    STEP_NR=$((STEP_NR+1))
}

clean_pyderivationagent_repository() {
    rm -rf $SPATH/build $SPATH/dist $SPATH/.eggs $SPATH/*egg-info $SPATH/*venv
}

install_pyderivationagent() {
    echo "-------------------------------"
    echo "Installing $PROJECT_NAME"
    echo "-------------------------------"
    clean_pyderivationagent_repository
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

run_pyderivationagent_tests() {
    echo "-------------------------------"
    echo "Running the $PROJECT_NAME tests"
    echo "-------------------------------"
    echo
    echo
    echo $1
    $1 -m pytest tests/ --docker-compose=./docker-compose.test.yml --reruns 5 --reruns-delay 5
    TEST_RESULT=$?
    if [ $TEST_RESULT -eq 0 ]; then
        echo "All tests have completed successfully"
    else
        echo "Aborting the release. Some tests have failed. Please check the pytest detailed output for more details."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    # compose down to ensure the containers are stopped
    docker compose -f "./docker-compose.test.yml" down
}

install_pyderivationagent_and_test() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Packaging the $PROJECT_NAME project"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    install_pyderivationagent $VENV_NAME $SPATH

    run_pyderivationagent_tests $PYTHON_EXEC

    STEP_NR=$((STEP_NR+1))
}

build_pyderivationagent_for_release() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Building the $PROJECT_NAME for the release"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    clean_pyderivationagent_repository
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
    $PYTHON_EXEC -m pip install pytest
    $PYTHON_EXEC -m pip install testcontainers
    $PYTHON_EXEC -m pip install pytest-docker-compose
    $PYTHON_EXEC -m pip install pytest-rerunfailures

    run_pyderivationagent_tests $PYTHON_EXEC

    echo "Removing test venv."
    rm -rf $SPATH/../$TEST_VENV_NAME

    STEP_NR=$((STEP_NR+1))
}

###############################
## functions for dev_release ##
###############################
dev_release() {
    dev_release_check_dev_version_number
    install_packages_for_building
    dev_release_bump_pyderivationagent_version_number
    dev_release_install_pyderivationagent_and_test
    build_pyderivationagent_for_release
    release_to_pypi test-pypi
    read -n 1 -s -r -p "Press any key to continue"
    exit
}

dev_release_check_dev_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Checking the $PROJECT_NAME dev version number"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    if [[ "$DEV_VERSION" =~ ^[0-9]*\.[0-9]*\.[0-9]*(a|b|rc)[0-9]*$ ]] # the version number should contain a, b or rc (alpha, beta or release candidate)
    then
        echo "Version number is correct. Proceeding with the dev release."
    else
        echo "The specified $PROJECT_NAME dev version number is $DEV_VERSION, which is NOT correct. It should contain a, b, or rc (alpha, beta or release candidate), e.g., 1.1.0b1. Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi

    STEP_NR=$((STEP_NR+1))
}

dev_release_bump_pyderivationagent_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Bumping the $PROJECT_NAME version number to $DEV_VERSION"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    sed -bi "s/version=.*,/version='$DEV_VERSION',/" $SPATH/setup.py
    sed -bi "s/__version__ = .*/__version__ = \"$DEV_VERSION\"/" $SPATH/$PROJECT_NAME/__init__.py

    STEP_NR=$((STEP_NR+1))
}

dev_release_install_pyderivationagent_and_test() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Packaging the $PROJECT_NAME project"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    install_pyderivationagent $VENV_NAME $SPATH

    dev_release_pyderivationagent_tests $PYTHON_EXEC

    STEP_NR=$((STEP_NR+1))
}

dev_release_pyderivationagent_tests() {
    echo "-------------------------------"
    echo "Running the $PROJECT_NAME tests"
    echo "-------------------------------"
    echo
    echo
    echo $1
    $1 -m pytest -s tests/test_conf.py tests/test_derivation_agent.py --reruns 5 --reruns-delay 5
    TEST_RESULT=$?
    if [ $TEST_RESULT -eq 0 ]; then
        echo "All tests have completed successfully"
    else
        echo "Aborting the release. Some tests have failed. Please check the pytest detailed output for more details."
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
        -v) NEXT_VERSION=$2; shift 2;;
        -d) DEV_VERSION=$2; shift 2; dev_release "$@";;
        *)
        # otherwise print the usage
        usage;;
    esac
done

main "$@"; exit

