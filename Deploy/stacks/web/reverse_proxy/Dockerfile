# Base image
#==================================================================================================
FROM nginx:1.19.6 as base
 
# Install cron
RUN apt-get update && apt-get install -y cron procps

# Install python3
RUN apt-get update && apt-get install -y python3

# Add the contents of conf.d
ADD conf.d /etc/nginx/conf.d/

# Rotate nginx logs daily
RUN mkdir /var/log/nginx/daily
COPY logrotate /etc/cron.daily/logrotate
RUN chmod 755 /etc/cron.daily/logrotate
COPY nginx /etc/logrotate.d/nginx
RUN chmod 644 /etc/logrotate.d/nginx

# Add cron.daily script
COPY parse-logs.sh /etc/cron.daily/parse-logs
RUN chmod 755 /etc/cron.daily/parse-logs

# Add python script
COPY log-parser.py /usr/local/log-parser.py
RUN chmod 777 /usr/local/log-parser.py

# Run both nginx and cron together on boot
CMD [ "sh", "-c", "cron && nginx -g 'daemon off;'" ]
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