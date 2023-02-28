FROM tiangolo/uwsgi-nginx-flask:python3.8 as flask
WORKDIR /app
#RUN apt-get update && \
#    apt-get upgrade -y && \
#    apt-get install -y git && \
#    git init && git remote add -f origin https://github.com/cambridge-cares/TheWorldAvatar \
#&& git sparse-checkout set MARIE_AND_BERT \
#&& git pull origin dev-marie-and-bert
RUN git clone --branch dev-marie-and-bert https://github.com/cambridge-cares/TheWorldAvatar
RUN cp -r TheWorldAvatar/MARIE_AND_BERT/Marie ./Marie  \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/uwsgi.ini ./uwsgi.ini \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/static ./static \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/templates ./templates \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/main.py ./main.py \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/requirements_linux.txt ./requirements_linux.txt \
&& cp -r TheWorldAvatar/MARIE_AND_BERT/prestart.sh ./prestart.sh
#COPY prestart.sh prestart.sh

#COPY requirements_linux.txt requirements_linux.txt
RUN apt-get update && \
    apt-get install -y openjdk-11-jdk && \
    apt-get install -y ant && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/ && \
    rm -rf /var/cache/oracle-jdk8-installer
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64/
RUN export JAVA_HOME
#COPY script.sh script.sh
RUN pip install STOUT-pypi==2.0.2
#RUN chmod +x ./script.sh
#CMD [ "/bin/bash","-c","./script.sh"]