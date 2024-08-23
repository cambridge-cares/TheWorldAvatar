#!/bin/bash
# J. Bai (jb2197@cam.ac.uk)
# M. Hofmeister (mh807@cam.ac.uk)
#
# Agent Docker image GitHub upload script
#
AUTHOR="Jiaru Bai <jb2197@cam.ac.uk>"
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
AGENT_NAME='dh-optimisation-trigger-agent'
CARES_GITHUB='ghcr.io/cambridge-cares/'
DOCKER_COMPOSE_FILE='docker-compose.yml'
STEP_NR=1
NEXT_VERSION=''
IMAGE_TAG=''

usage() {
    echo "==============================================================================================================="
    echo $AGENT_NAME" docker image GitHub upload script."
    echo
    echo "Please run the script with following options:"
    echo "---------------------------------------------------------------------------------------------------------------"
    echo " Usage:"
    echo "  -v NEXT_VERSION"
    echo "  -h"
    echo ""
    echo "Options"
    echo "  -v              : Upload the $AGENT_NAME to GitHub repo with the following version."
    echo "  -h              : Print this usage message."
    echo ""
    echo "Example usage:"
    echo "./publish_docker_image.sh -v 1.0.0            - upload version 1.0.0"
    echo "./publish_docker_image.sh -v 1.0.0-SNAPSHOT   - upload version 1.0.0-SNAPSHOT"
    echo "==============================================================================================================="
    read -n 1 -s -r -p "Press any key to continue"
    exit
}

main() {
    bump_agent_docker_image_version_number
    build_agent_docker_image
    upload_docker_image_to_github
    read -n 1 -s -r -p "Press any key to continue"
}

bump_agent_docker_image_version_number() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Bumping the $AGENT_NAME version number to $NEXT_VERSION"
    echo "-------------------------------------------------------------------------"
    echo ; echo

    echo "upload version $NEXT_VERSION"
    IMAGE_TAG=$CARES_GITHUB$AGENT_NAME:$NEXT_VERSION
    echo "Image tag: $IMAGE_TAG"
    sed -bi "s|image:.*|image: $IMAGE_TAG|" $SPATH/$DOCKER_COMPOSE_FILE

    STEP_NR=$((STEP_NR+1))
}

build_agent_docker_image() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Building the $AGENT_NAME docker image and tagging it as $IMAGE_TAG"
    echo "-------------------------------------------------------------------------"
    echo ; echo
    docker compose -f $SPATH/$DOCKER_COMPOSE_FILE build

    STEP_NR=$((STEP_NR+1))
}

upload_docker_image_to_github() {
    echo "-------------------------------------------------------------------------"
    echo "$STEP_NR. Uploading $IMAGE_TAG to GitHub repo"
    echo "-------------------------------------------------------------------------"
    echo ; echo
    read -p "Enter your username: " username

    docker login ghcr.io --username $username

    docker push $IMAGE_TAG

    if [ $? -ne 0 ]; then
        echo "Couldnt upload artifacts to $1. Have you forgotten to increse the $AGENT_NAME version number?"
        echo "Aborting the release."
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
        *)
        # otherwise print the usage
        usage;;
    esac
done

main "$@"; exit
