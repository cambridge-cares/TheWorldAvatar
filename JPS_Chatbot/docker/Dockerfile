FROM ubuntu:20.04
ARG DEBIAN_FRONTEND=noninteractive
MAINTAINER xz378@cam.ac.uk
RUN apt-get update
CMD ["echo", "Hello from the docker"]
# RUN apt-get install git
# RUN git --version
RUN apt-get install git -y
RUN apt-get install python3.6 -y
RUN apt-get install expect -y
RUN apt-get install vim -y

CMD ["echo", "Hello from the docker 2"]
ADD git_clone.sh  .
ADD git_checkout.sh . 
ADD download_model.sh . 
RUN /usr/bin/expect git_clone.sh
RUN /bin/bash git_checkout.sh
# RUN cd JParkSimulator-git
# RUN git checkout jps-chatbot
RUN /usr/bin/expect download_model.sh
RUN apt update
RUN apt install software-properties-common -y
RUN add-apt-repository ppa:deadsnakes/ppa -y
RUN apt install python3.7 -y
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.7 1
RUN apt install python3-pip -y
RUN pip3 install virtualenv
RUN apt install -y build-essential libssl-dev libffi-dev python3-dev
RUN (cd JParkSimulator-git/JPS_Chatbot; python3.7 -m pip install -r requirements.txt)
RUN apt-get install wget
RUN wget https://github.com/mozilla/geckodriver/releases/download/v0.24.0/geckodriver-v0.24.0-linux64.tar.gz
RUN tar -xvzf geckodriver*
RUN chmod +x geckodriver
RUN export PATH=$PATH:.
RUN apt install firefox -y
RUN cp geckodriver /usr/local/bin
ADD start_flask.sh .
# ADD flask.service /etc/systemd/system/
ADD install_nltk.py .
RUN python install_nltk.py
ADD flask /etc/init.d/

RUN apt install dos2unix -y
RUN dos2unix /etc/init.d/flask
RUN chmod +x /etc/init.d/flask
RUN update-rc.d flask defaults
# RUN /bin/bash service flask start
# RUN systemctl start flask
# RUN (cd /JParkSimulator-git/JPS_Chatbot/UI/source; nohup python -m flask run)
# ADD flask.conf /etc/init
# CMD ["echo", "Hello from the docker 3"]