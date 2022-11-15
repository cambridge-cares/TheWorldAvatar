#! /bin/bash

#NOTE: the port number must match that in docker-compose
accessagentURL=http://localhost:48888/access-agent/upload

#Routing information
routingInfo=./routing.json

## Upload routing information 
echo "Uploading routing information to $accessagentURL"

# Upload triples to blazegraph
curl -X POST --data-binary @$routingInfo --header 'Content-Type:application/json' $accessagentURL

echo "Done!"