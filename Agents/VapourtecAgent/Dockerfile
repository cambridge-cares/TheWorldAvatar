# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

# Base image is a lightweight version of Python
FROM python:3.8-slim-buster

# Meta data
LABEL authors = "jb2197@cam.ac.uk"
LABEL description = "Execute the requested reaction experiment."

# Install required software: git and openjdk-11
USER root
RUN apt update && apt install -y git openjdk-11-jdk-headless

# Required to install, otherwise will thorw below error when install Mono:
# #8 0.390 E: gnupg, gnupg2 and gnupg1 do not seem to be installed, but one of them is required for this operation
RUN apt-get update && apt-get install -y gnupg

# Mono and pythonnet install bit taken from:
# https://github.com/jonemo/pythonnet-docker/blob/master/archived-dockerfiles/python3.8.3-mono5.20-pythonnet2.5.0
# Mono: 5.20
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF \
  && echo "deb http://download.mono-project.com/repo/debian stretch/snapshots/5.20 main" > /etc/apt/sources.list.d/mono-official.list \
  && apt-get update \
  && apt-get install -y clang \
  && apt-get install -y mono-devel=5.20\* \
  && rm -rf /var/lib/apt/lists/* /tmp/*

# Below is to also include gcc to the image, otherwise will throw error:
# unable to execute 'gcc': No such file or directory
RUN apt-get update \
  && apt-get install -y gcc \
  && apt-get install -y g++

# Pythonnet: 2.5.0 (from PyPI)
# Note: pycparser must be installed before pythonnet can be built
RUN pip install pycparser \
  && pip install pythonnet==2.5.0

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install -r requirements.txt

# Set the default working directory, then copy the Python source code into it
WORKDIR /app
ADD vapourtecagent /app/vapourtecagent

# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
RUN useradd appuser && chown -R appuser /app

# Creating the user doesn't create their home directory for some reason, so create it now
RUN mkdir /home/appuser
RUN chown -R appuser:appuser /home/appuser
RUN chmod -R 755 /home/appuser

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'python_agent' module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
USER appuser
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "vapourtecagent:create_app()"]
