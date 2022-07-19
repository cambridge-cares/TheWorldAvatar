#! /bin/bash

## Variables
routerEndpoint="http://localhost:48889/bigdata/dataloader" #NOTE: the port number must match that in docker-compose

dataFile="dataloader.txt"
prefix="<http://www.theworldavatar.com/kb/ontokgrouter/"

####################################

## Run docker-compose 
echo "Spinning up the AccessAgent stack"
#docker-compose up -d --no-build

####################################

## Upload routing information 
echo "Uploading routing information to $routerEndpoint"

# Get routing information
. ./config/ontokgrouter.properties

# Clear file
rm $dataFile

# Write routing information to triples
echo "${prefix}${label}> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint> \"${queryEndpoint}\"." >> $dataFile
echo "${prefix}${label}> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> \"${updateEndpoint}\"." >> $dataFile
echo "${prefix}${label}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>." >> $dataFile
echo "${prefix}${label}> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>." >> $dataFile
echo "${prefix}${label}> <http://www.w3.org/2000/01/rdf-schema#label> \"${label}\"." >> $dataFile

# Upload triples to blazegraph
#curl -X POST --data-binary @$data --header 'Content-Type:text/plain' $routerEndpoint

echo "Done!"