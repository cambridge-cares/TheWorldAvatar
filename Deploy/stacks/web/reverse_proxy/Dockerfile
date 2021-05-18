# Base image (HTTP only)
FROM nginx:1.19.6 as no_ssl
 
# Install cron
RUN apt-get update && apt-get install -y cron

# Add the contents of conf.d
ADD conf.d /etc/nginx/conf.d/

# Copy in no-ssl configuration 
COPY nginx_no_ssl.conf /etc/nginx/nginx.conf

# Rotate nginx logs daily
RUN mkdir /var/log/nginx/daily
COPY logrotate /etc/cron.daily/logrotate
RUN chmod 755 /etc/cron.daily/logrotate
COPY nginx /etc/logrotate.d/nginx
RUN chmod 644 /etc/logrotate.d/nginx

# Add script to start cron
COPY start-cron.sh /docker-entrypoint.d/30-start-cron.sh
RUN chmod 777 /docker-entrypoint.d/30-start-cron.sh

# Add cron.daily script
COPY parse-logs.sh /etc/cron.daily/parse-logs.sh
RUN chmod 755 /etc/cron.daily/parse-logs.sh

# Add python script
COPY log-parser.py /usr/local/log-parser.py
RUN chmod 777 /usr/local/log-parser.py

# Extended image (HTTPS support)
FROM no_ssl as with_ssl

# Copy in with-ssl configuration
COPY nginx_with_ssl.conf /etc/nginx/nginx.conf