# Base image
#==================================================================================================
FROM nginx:1.19.6 as base

# Add the contents of conf.d
ADD conf.d /etc/nginx/conf.d/
#==================================================================================================


# Dev image
#==================================================================================================
FROM base as dev

# Copy in dev configuration file - includes dev-specific servers (doesn't use SSL)
COPY nginx.dev.conf /etc/nginx/nginx.conf
#==================================================================================================


# Prod image
#==================================================================================================
FROM base as prod

# Copy in prod configuration file - includes prod-specific servers and enables SSL
COPY nginx.prod.conf /etc/nginx/nginx.conf
#==================================================================================================