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

FROM node:18.18-slim

# Make directory to house application
RUN mkdir /twa

# Copy in application files
COPY ./code/ /twa

# Expose port 80
EXPOSE 80

# Build then run the app
WORKDIR /twa
CMD [ "/bin/bash", "-c", "npm install && npm run dev" ]
