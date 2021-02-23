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
#	production			Required software only
#
# NOTE: the below files are required to be in the same
# directory as this file before execution (these files
# should be available already in the JPS git repository.)
#
#	download_model.sh	(Downloads the trained model files)
# 	start_flask.sh 		(Starts the flask process)
# 	install_nltk.py		(Installs the NLTK module)
#
#########################


##### PRODUCTION STAGE #####
# Use the 20.04 Ubuntu image as a base
FROM ubuntu:20.04 as production
ARG DEBIAN_FRONTEND=noninteractive

# Updates and software installation
RUN echo -e "Installing required software..."
RUN apt-get update
RUN apt-get install git -y
RUN apt-get install expect -y
RUN apt-get install software-properties-common -y
RUN apt-get install dos2unix -y
RUN echo -e "Required software installed."

# Set installation directory
RUN mkdir -p /usr/local/jps-chatbot
WORKDIR /usr/local/jps-chatbot

# Copy in the JPS_Chatbot folder
COPY . .
RUN chmod +x ./docker/start_flask.sh

# Run scripts
RUN echo -e "Downloading models..."
RUN /usr/bin/expect docker/download_model.sh
RUN rm -f docker/download_model.sh
RUN echo -e "Model download completed."

# Python setup
RUN echo -e "Installing Python..."
RUN apt-get install python3.6 -y
RUN add-apt-repository ppa:deadsnakes/ppa -y
RUN apt-get install python3.7 -y
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.7 1
RUN apt-get install python3-pip -y
RUN pip3 install virtualenv
RUN apt-get install -y build-essential libssl-dev libffi-dev python3-dev
RUN echo -e "Python installed."

# Geckodriver and Firefox setup
RUN apt-get install wget -y
RUN wget https://github.com/mozilla/geckodriver/releases/download/v0.24.0/geckodriver-v0.24.0-linux64.tar.gz
RUN tar -xvzf geckodriver*
RUN chmod +x geckodriver
RUN cp geckodriver /usr/local/bin
RUN apt-get install firefox -y

# JPS Chatbot installation
RUN echo -e "Installing the chatbot..."
RUN (python3.7 -m pip install -r requirements.txt)
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

# Reset arguments and finish
ARG DEBIAN_FRONTEND=newt
RUN echo -e "Image installation completed."

# Set the entrypoint (start flask on container creation)
ENTRYPOINT ["/bin/bash", "-c", "cd /usr/local/jps-chatbot/UI/source && python -m flask run --host=0.0.0.0"]
