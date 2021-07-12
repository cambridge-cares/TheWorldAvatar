#########################
#
# This docker file creates an image for the JPS Chatbot.
#
# NOTE: The "docker build" command used to build this file
# into a image should be run from the JPS_Chatbot folder,
# not from within the "docker" directory. See the README
# for more details.
# 
# The following stages are included:
# 
#	credentials			Downloads protected files
#	production			Required software only
#
#########################


##### CREDENTIALS STAGE #####
FROM alpine:3.13.4 as credentials

RUN apk update
RUN apk add expect
RUN apk add openssh-client


# Copy in the credentials folder
COPY ./docker/credentials /tmp/credentials
WORKDIR /tmp/credentials

# Locations for file downloads
RUN mkdir /tmp/models_wiki
RUN mkdir /tmp/models_jps/

# Run download script
RUN /usr/bin/expect ./download-models.sh

##### PRODUCTION STAGE #####
# Use the 20.04 Ubuntu image as a base
FROM ubuntu:20.04 as production
ARG DEBIAN_FRONTEND=noninteractive

# Updates and software installation
RUN echo -e "Installing required software..."
RUN apt-get update && apt-get install -y git dos2unix software-properties-common

# Set the locale to UK 
RUN apt-get -y install locales
RUN sed -i '/en_GB.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_GB.UTF-8  
ENV LANGUAGE en_GB:en  
ENV LC_ALL en_GB.UTF-8  


# Set installation directory
RUN mkdir -p /usr/local/jps-chatbot
WORKDIR /usr/local/jps-chatbot

# Copy in the JPS_Chatbot folder
COPY . .
RUN chmod +x ./docker/start_flask.sh

# Copy in the model files downloaded in last stage
COPY --from=credentials /tmp/models_wiki /usr/local/jps-chatbot/UI/source/Wikidata_Query
COPY --from=credentials /tmp/models_jps /usr/local/jps-chatbot/UI/source/JPS_Query


# Python setup
RUN echo -e "Installing Python..."
RUN add-apt-repository ppa:deadsnakes/ppa -y
RUN apt-get install -y python3.6 python3.7

RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.7 1
RUN apt-get install python3-pip -y
RUN pip3 install virtualenv
RUN apt-get install -y build-essential libssl-dev libffi-dev python3-dev
RUN echo -e "Python installed."

# Geckodriver and Firefox setup
RUN apt-get install wget -y
RUN wget https://github.com/mozilla/geckodriver/releases/download/v0.24.0/geckodriver-v0.24.0-linux64.tar.gz
RUN tar -xvzf geckodriver*.gz
RUN chmod +x geckodriver
RUN cp geckodriver /usr/local/bin
RUN apt-get install firefox -y --fix-missing

# JPS Chatbot installation
RUN echo -e "Installing the chatbot..."
RUN apt-get install mysql-server -y
RUN apt-get install libmysqlclient-dev -y 
RUN (python3.7 -m pip install -r requirements.txt)
# RUN (python3.7 -m pip install rapidfuzz)
# RUN (python3.7 -m pip install yagmail)
RUN export PATH=$PATH:.
RUN python docker/install_nltk.py
RUN echo -e "Chatbot installed..."

# Flask setup and execution
RUN echo -e "Setting up Flask..."
ADD docker/start_flask.sh .
ADD docker/flask /etc/init.d/
RUN dos2unix /etc/init.d/flask
RUN chmod +x /etc/init.d/flask
RUN update-rc.d flask defaults
RUN echo -e "Flask started."

# Set up crontab 
# RUN apt-get update \
#    && apt-get install -y cron \
#    && apt-get autoremove -y

# COPY ./docker/cronpy /etc/cron.d/cronpy
# CMD ["cron", "-f"]


# Reset arguments and finish
ARG DEBIAN_FRONTEND=newt
RUN echo -e "Image installation completed."

# Set the entrypoint (start flask on container creation)
ENTRYPOINT ["/bin/bash", "-c", "cd /usr/local/jps-chatbot/UI/source && python -m flask run --host=0.0.0.0"]
