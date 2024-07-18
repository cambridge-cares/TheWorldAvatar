#
# Dockerfile for TWA-VF build environment.
#

from ubuntu:23.10 as twavf-build

# Install latest NodeJS and NPM
RUN apt update && apt install -y nodejs npm

# Install Grunt
RUN npm install -g grunt-cli

# Install Typescript
RUN npm install -g typescript