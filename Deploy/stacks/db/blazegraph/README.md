The container image defined here is built using the latest release candidate of the Blazegraph triplestore.

Version 1.1.0 of the image introduces the ability to enable BASIC authentication when starting the container.
To enable this the following environment variables need to have been specified:
* ```BLAZEGRAPH_USER```: The user name that needs to be entered.
* ```BLAZEGRAPH_PASSWORD_FILE```: The path of a file containing the password within the container (should generally be ```/run/secrets/blazegraph_password```)

Also a secret with the same name as the file name specified in the ```BLAZEGRAPH_PASSWORD_FILE``` path needs to be defined and added to the container.

The relevant entries in the Docker compose file [docker-compose.deploy.yml](../docker-compose.deploy.yml) are commented out by default.