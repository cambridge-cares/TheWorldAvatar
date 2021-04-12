#!/bin/sh

# Script for docker that stops everything and removes all Containers, Images, and Volumes.
# THIS IS IRREVERSIBLE AND SHOULD BE USED WITH CAUTION

read -p "This will remove all local containers, images, and volumes. Continue? (y/n) " -n 1 -r
echo    # (optional) move to a new line
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
	printf "Stopping script."
    exit 1
fi

printf "Starting script (may show errors if no containers/images/volumes exist)...\n"

printf "Stopping all containers...\n"
docker stop $(docker ps -aq)

printf "Removing all containers...\n"
docker rm --force $(docker ps -aq)

printf "Removing all images...\n"
docker rmi --force $(docker images -q)

printf "Removing all volumes...\n"
docker volume rm --force $(docker volume ls -q)

printf "Removal complete."