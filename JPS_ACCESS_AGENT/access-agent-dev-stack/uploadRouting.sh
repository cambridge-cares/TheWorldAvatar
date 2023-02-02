#! /bin/bash

# NB: The port number must match that in docker-compose, or, if running
# within a stack, the main entry port to the stack, e.g. 3838.
accessagentURL=http://localhost:48888/access-agent/upload

# Routing information
routingInfo=./routing.json

# Upload routing information 
echo "Uploading routing information to $accessagentURL"

# Upload triples to blazegraph
curl -X POST --data-binary @$routingInfo --header 'Content-Type:application/json' $accessagentURL

echo "Done!"
