FROM docker.cmclinnovations.com/nginx:1.19.6 as no_ssl

# Arguments
ARG HASH

# Labels
LABEL authors = "support@cmclinnovations.com"
LABEL description = "nginx, configured as a reverse proxy for all web services that need to be made available externally."
LABEL hash = $HASH

# Add the contents of conf.d
ADD conf.d /etc/nginx/conf.d/

# Copy in no-ssl configuration 
COPY nginx_no_ssl.conf /etc/nginx/nginx.conf


FROM no_ssl as with_ssl

# Copy in with-ssl configuration
COPY nginx_with_ssl.conf /etc/nginx/nginx.conf
