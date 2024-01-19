# Public FTP Server

This directory contains the files required to build a Docker image that houses an [FTPS server](https://www.pureftpd.org/project/pure-ftpd/) and an [Apache webserver](https://httpd.apache.org/).

This image is intended to be built into a container that will sit alongside a website (perhaps behind a reverse proxy) and act as a CDN for publicly available JavaScript libraries. As such, nothing added to this FTP server should be considered protected in any way.

For more details on Docker usage within TheWorldAvatar, including how to access scripts to build and generate container from existing stacks, please see the [Docker Stacks](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker:-Stacks) page of the wiki.

## Configuration

This image contans the following technologies:

- **Pure FTPD:** This is used to provide an FTPS server with access via a single account specified at build time. The FTP user space is restricted to the `/mount/ftp` directory.
- **Apache Webserver:** This is used to host content from the `/mount/ftp` directory, allowing anonymous visitors to download and browse files (no write access).

The `/mount/ftp` has also been configured as a [Docker Volume](https://docs.docker.com/storage/volumes/) to ensure file persistence when updating/restaring containers.

## Building
Follow these steps to build the source code into a new Docker image:

- Create a `variables.env` file within the current directory. This is a key-value file that is used to set environment variables within the container.
- Add `FTP_USER_NAME` and `FTP_USER_PASS` entries to this file. These will set the credentials required to FTP into the container.
   - For details on credentials of existing containers, contact the support team at CMCL Innovations.
- Build the `public-ftp` service as part of the existing `web` stack. Note that this service only exists in the production stack.

## Running

As part of the wider `web` stack, containers can be created using the existing `start-stack.sh` script as part of the entire stack, or in isolation.