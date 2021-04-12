# centos7-systemd

This base image uses the Docker Hub CentOS 7 image and adds support for systemd.

## Building

To build the base image, simply use the `docker-compose build --force-rm --no-cache` command from within this directory.

## Pushing

To push the base image to the Nexus server at CMCL...

* Ensure you've used the `docker login -u USERNAME -p PASSWORD docker.cmclinnovations.com` command to register your credentials.
* Run the `docker image push docker.cmclinnovations.com/centos7-systemd:latest` command.