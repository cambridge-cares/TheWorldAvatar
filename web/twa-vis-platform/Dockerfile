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

# ---- Development Image ----
FROM node:18.18-slim AS develop

# Make directory to house application
RUN mkdir -p /twa
WORKDIR /twa

# Copy only the package.json to install the required dependencies
COPY ./code/package.json /twa/package.json
RUN npm install 

# Next.js collects completely anonymous telemetry data about general usage and should be disabled
ENV NEXT_TELEMETRY_DISABLED 1

CMD [ "/bin/bash", "-c", "npm run dev" ]

# ---- Production Image ----
FROM node:18 AS prod

# Next.js collects completely anonymous telemetry data about general usage and should be disabled
ENV NEXT_TELEMETRY_DISABLED 1

# Copy and make start up script executable
COPY ./start-up.sh /usr/local/bin/start-up.sh
RUN chmod +x /usr/local/bin/start-up.sh

RUN mkdir -p /twa
WORKDIR /twa

# Copy all code files
COPY ./code/ ./

EXPOSE 3000
ENV PORT 3000
# set hostname to localhost
ENV HOSTNAME "0.0.0.0"

CMD ["/usr/local/bin/start-up.sh"]