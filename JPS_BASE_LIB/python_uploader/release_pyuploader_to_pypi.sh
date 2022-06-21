#!/bin/bash
# D. Nurkowski (danieln@cmclinnovations.com)
#
# package release script
#
AUTHOR="Daniel Nurkowski <danieln@cmclinnovations.com>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
VENV_NAME='pyuploader_venv'
TEST_VENV_NAME='test_venv'
PROJECT_NAME='pyuploader'
TEST_PYPI="https://test.pypi.org/legacy/"
DEP_FILE='dev_requirements.txt'
STEP_NR=1
NEXT_VERSION=''
PYPI_WAIT_TIME_SEC=300

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
    echo "./release_pyuploader_to_pypi.sh -v 1.0.14   - release version 1.0.14"
	echo "==============================================================================================================="
	read -n 1 -s -r -p "Press any key to continue"
    exit
}

main() {
    bump_package_version_number
    install_and_test_package
    build_package_for_release
    release_package_to_pypi test-pypi
    test_package_release test-pypi $PYPI_WAIT_TIME_SEC
    release_package_to_pypi main-pypi
    test_package_release main-pypi $PYPI_WAIT_TIME_SEC
    read -n 1 -s -r -p "Press any key to continue"
}

bump_package_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Bumping the $PROJECT_NAME version number to $NEXT_VERSION"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    sed -bi "s/version=.*,/version='$NEXT_VERSION',/" $SPATH/setup.py

    STEP_NR=$((STEP_NR+1))
}

clean_package_repository() {
    rm -rf $SPATH/build $SPATH/dist $SPATH/.eggs $SPATH/*egg-info $SPATH/*venv
}

install_and_test_package() {
    echo "-------------------------------"
    echo "$STEP_NR. Installing $PROJECT_NAME"
    echo "-------------------------------"
    clean_package_repository
    sleep .5

    $SPATH/install_script_pip.sh -v -i -e -n $VENV_NAME -d $SPATH -s

    if [ -d "$SPATH/$VENV_NAME/bin/pip3" ]; then
        PYTHON_EXEC=$SPATH/$VENV_NAME/bin/python
    else
        PYTHON_EXEC=$SPATH/$VENV_NAME/Scripts/python
    fi

    run_package_tests $PYTHON_EXEC

    STEP_NR=$((STEP_NR+1))
}

run_package_tests() {
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

build_package_for_release() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Building the $PROJECT_NAME for the release"
    echo "-------------------------------------------------------------------------"
    echo
    echo
    clean_package_repository
    sleep .5
    python setup.py sdist bdist_wheel
    if [ $? -eq 0 ]; then
        echo "Build successfull. Checking the distribution artifacts."
        twine check dist/*
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

release_package_to_pypi() {
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
        twine upload -u $username -p $password $SPATH/dist/*
    else
        echo "test release"
        twine upload -u $username -p $password --repository-url $TEST_PYPI $SPATH/dist/*
    fi
    if [ $? -ne 0 ]; then
        echo "Couldnt upload artifacts to $1. Have you forgotten to increse the $PROJECT_NAME version number?"
        echo "Aborting the release."
        read -n 1 -s -r -p "Press any key to continue"
        exit -1
    fi
    STEP_NR=$((STEP_NR+1))
}

test_package_release() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Testing the $PROJECT_NAME $1 release."
    echo "-------------------------------------------------------------------------"
    echo
    echo

    echo "Creating test venv."
    rm -rf $SPATH/../$TEST_VENV_NAME
    sleep .5
    python -m venv $SPATH/../$TEST_VENV_NAME

    if [ -d "$SPATH/../$TEST_VENV_NAME/bin/pip3" ]; then
        PYTHON_EXEC=$SPATH/../$TEST_VENV_NAME/bin/python
    else
        PYTHON_EXEC=$SPATH/../$TEST_VENV_NAME/Scripts/python
    fi

    echo "Waiting $2 second(s) for the $1 to update its package index"
    wait_n_seconds $2

    $PYTHON_EXEC -m pip install --upgrade pip
    if [ $1 = "test-pypi" ]; then
        $PYTHON_EXEC -m pip install --no-cache-dir --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple "$PROJECT_NAME==$NEXT_VERSION"
    else
        $PYTHON_EXEC -m pip install --no-cache-dir --upgrade "$PROJECT_NAME==$NEXT_VERSION"
    fi
    $PYTHON_EXEC -m pip install -r $SPATH/dev_requirements.txt

    run_package_tests $PYTHON_EXEC

    echo "Removing test venv."
    echo "Waiting 20 second(s) before the test venv removal"
    wait_n_seconds 20
    rm -rf $SPATH/../$TEST_VENV_NAME

    STEP_NR=$((STEP_NR+1))
}

wait_n_seconds() {
    steps=20
    sleeptime_sec=$1
    sleeptime_sec_step=$((sleeptime_sec/$steps))
    echo -ne "(0%)\r"
    for step in `seq $steps`
        do
            sleep $sleeptime_sec_step
            progress_prec=$((100/$steps*$step))
            progress="($progress_prec%)   "
            for a in `seq 1`;do progress_bar=$progress_bar$(echo -ne \#); done
            progress=$progress$progress_bar"\r"
            echo -ne $progress
        done
    echo -ne '\n'
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
