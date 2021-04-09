# Base image (HTTP only)
FROM nginx:1.19.6 as no_ssl
 
# Install cron
RUN apt-get update && apt-get install -y cron procps nano

# Add the contents of conf.d
ADD conf.d /etc/nginx/conf.d/

# Copy in no-ssl configuration 
COPY nginx_no_ssl.conf /etc/nginx/nginx.conf


# Log configuration
RUN mkdir /var/log/nginx/store
COPY logrotate /etc/cron.hourly/logrotate
COPY nginx /etc/logrotate.d/nginx

# Add script to start cron
COPY start-cron.sh /docker-entrypoint.d/30-start-cron.sh
RUN chmod 777 /docker-entrypoint.d/30-start-cron.sh
RUN echo "hello world"

# Extended image (HTTPS support)
FROM no_ssl as with_ssl

# Copy in with-ssl configuration
COPY nginx_with_ssl.conf /etc/nginx/nginx.conf