# set base image
# Base image is a lightweight version of Python
FROM python:3.9-slim-buster

# Install required software
RUN apt update && apt install -y git

# Install Java
RUN apt update && apt install -y openjdk-11-jdk-headless

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# set the working directory in the container
WORKDIR /root/ontomatch-py

# copy the dependencies file to the working directory
COPY requirements.txt .

# install dependencies
RUN pip install -r requirements.txt

# copy the content of the local src directory to the working directory
COPY ./ .
COPY ./conf/http_test/conf_test_linking.json  /root

#required pre-downloads
RUN python -m nltk.downloader stopwords
RUN python -m nltk.downloader wordnet

ENTRYPOINT ["python",  "./flaskapp/wsgi.py"]