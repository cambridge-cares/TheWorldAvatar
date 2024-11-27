#!/bin/sh

# Read secrets from files and export as environment variables
MAPBOX_USERNAME="$(cat /run/secrets/mapbox_username)"
export MAPBOX_USERNAME
MAPBOX_API_KEY="$(cat /run/secrets/mapbox_api_key)"
export MAPBOX_API_KEY

# Start development container
pnpm run dev-docker