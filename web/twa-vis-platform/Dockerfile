#
# This Dockerfile copies in the node project's source, and sets the 
# Docker starting command to run when the container launches. 
#
# Note that it does not actually build the project until the container
# is actually executed. This is because Next.js performs its Static
# Site Generation (server-side rendering) at build time; building
# during the Image build process would mean that no data in any mounted
# volumes could be used, so instead we build when the container launches,
# so that volumed data can be used.
#

FROM node:18.18-slim as base

# Make directory to house application
RUN mkdir -p /twa/app
WORKDIR /twa/app

# Copy only the package.json to install the required dependencies
COPY ./code/package.json /twa/app/package.json
RUN npm install 

# ---- For development ----
FROM base as develop
ENV UPLOADS_PATH /twa/uploads
CMD [ "/bin/bash", "-c", "npm run dev" ]
