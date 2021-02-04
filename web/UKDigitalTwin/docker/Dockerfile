#########################
#
# This docker file creates an image for the UK Digital Twin.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the UKDigitalTwin folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# The following stages are included:
# 
#	production			Required software only
#
#########################

##### PRODUCTION STAGE #####
# Use the Node as a base
FROM node:current-alpine3.12 as production

# Meta data
LABEL authors = "wx243@cam.ac.uk, support@cmclinnovations.com"
LABEL version = "0.9.2"
LABEL description = "UK Digital Twin"

# Updates and software installation
RUN echo -e "Installing required software..."
RUN apk update
RUN apk upgrade
RUN apk add build-base
RUN apk add python3 py3-pip
RUN apk add dpkg
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3 1
RUN apk add redis
RUN npm install pm2@latest -g
RUN echo -e "Required software installed."

# Set installation directory
RUN mkdir -p /usr/local/uk-digital-twin
WORKDIR /usr/local/uk-digital-twin

# Copy in the UKDigitalTwin folder
COPY . .
RUN chmod +x docker/start-twin.sh

ENV NODE_ENV=CMCLDeploy
RUN npm install

# Finish
RUN echo -e "Image installation completed."

# Set the entrypoint
ENTRYPOINT ["/bin/bash", "-c", "redis-server --daemonize yes && pm2-runtime app.js --name UKDigitalTwin"]

##### DEVELOPMENT STAGE #####
# Use the Node as a base
FROM production as development

RUN apk add bash
RUN apk add nano