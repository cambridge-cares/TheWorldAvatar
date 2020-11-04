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

