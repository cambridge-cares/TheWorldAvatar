##### Base configuration #####
FROM nginx:1.19.6 as base

# Meta data
LABEL authors = "support@cmclinnovations.com"
LABEL version = "0.0.1"
LABEL description = "Reverse Proxy"

##### Dev environment config #####
FROM base as dev

# Copy in dev configuration file
COPY dev.conf /etc/nginx/conf.d/default.conf
RUN chmod 644 /etc/nginx/conf.d/default.conf

RUN apt-get update
RUN apt-get install nano