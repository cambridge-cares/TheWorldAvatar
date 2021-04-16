# From the Ubuntu base image
FROM ubuntu:20.04

# Requirements
RUN apt-get update && apt-get install -y lsb-release cron wget gnupg

# Add GoAccess repository and install
RUN echo "deb https://deb.goaccess.io/ $(lsb_release -cs) main" | tee -a /etc/apt/sources.list.d/goaccess.list
RUN wget -O - https://deb.goaccess.io/gnugpg.key | apt-key --keyring /etc/apt/trusted.gpg.d/goaccess.gpg add -
RUN apt-get update && apt-get install -y goaccess

# Add the script to generate the report
COPY generate-report.sh /etc/cron.daily/goaccess
RUN chmod 755 /etc/cron.daily/goaccess

# Place for generated HTML report
RUN mkdir /var/log/goaccess

# Add script to start cron
COPY start-cron.sh /docker-entrypoint.d/30-start-cron.sh
RUN chmod 777 /docker-entrypoint.d/30-start-cron.sh