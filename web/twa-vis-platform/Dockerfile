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
RUN mkdir -p /twa
WORKDIR /twa

# Copy only the package.json to install the required dependencies
COPY ./code/package.json /twa/package.json
RUN npm install 

# Next.js collects completely anonymous telemetry data about general usage and should be disabled
ENV NEXT_TELEMETRY_DISABLED 1

# ---- Development Image ----
FROM base as develop

CMD [ "/bin/bash", "-c", "npm run dev" ]

# ---- For production ----
# Building the code
FROM base as builder

ENV NODE_ENV production

# Copy the required code
COPY ./code/ /twa/
# Transfer the uploads so that the building process can complete
COPY ./uploads/config /twa/uploads/config
COPY ./uploads/optional-pages /twa/uploads/optional-pages
COPY ./uploads/style-overrides.css /twa/uploads/style-overrides.css

# Compile the files
RUN npm run build

# ---- Production Image ----
FROM node:18-alpine AS prod

# Environment variables
ENV NODE_ENV production
# Next.js collects completely anonymous telemetry data about general usage and should be disabled
ENV NEXT_TELEMETRY_DISABLED 1

WORKDIR /twa

# Install only production dependencies
COPY ./code/package*.json ./
RUN npm install

# Create specific user in a user group
RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

# Copy the public resources
COPY --from=builder /twa/public ./public

# Set the correct permission for prerender cache
RUN mkdir .next
RUN chown nextjs:nodejs .next

# Automatically leverage output traces to reduce image size
COPY --from=builder --chown=nextjs:nodejs /twa/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /twa/.next/static ./.next/static

# Set up environment for starting server
USER nextjs

EXPOSE 3000
ENV PORT 3000
# set hostname to localhost
ENV HOSTNAME "0.0.0.0"

# server.js is created by next build from the standalone output
# https://nextjs.org/docs/pages/api-reference/next-config-js/output
CMD ["node", "server.js"]