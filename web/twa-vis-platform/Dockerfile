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

# ---- Base Image ----
FROM node:22.4-slim AS base
ENV PNPM_HOME="/pnpm"
ENV PATH="$PNPM_HOME:$PATH"
RUN corepack enable
WORKDIR /twa
# Next.js collects completely anonymous telemetry data about general usage and should be disabled
ENV NEXT_TELEMETRY_DISABLED 1
COPY ./code/package.json ./code/pnpm-lock.yaml ./

# ---- Production Dependencies ----
FROM base AS prod-deps
RUN --mount=type=cache,id=pnpm,target=/pnpm/store pnpm install --prod --frozen-lockfile

# ---- Development Dependencies ----
FROM base AS dev-deps
RUN --mount=type=cache,id=pnpm,target=/pnpm/store pnpm install --frozen-lockfile

# ---- Development Image ----
FROM base AS develop
COPY --from=dev-deps /twa/node_modules ./node_modules
COPY ./code ./
CMD [ "/bin/bash", "-c", "pnpm run dev-docker" ]

# ---- Build ----
FROM base AS build
COPY --from=dev-deps /twa/node_modules ./node_modules
COPY ./code ./
RUN pnpm run build

# ---- Production Image ----
FROM base AS production
COPY --from=prod-deps /twa/node_modules ./node_modules
COPY --from=build /twa/.next ./.next
COPY ./code ./
EXPOSE 3000
ENV HOSTNAME "0.0.0.0"
CMD [ "pnpm", "start" ]