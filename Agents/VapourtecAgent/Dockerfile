# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

# Base image is a lightweight version of Python
FROM python:3.7-slim-buster

# Install required software
RUN apt update && apt install -y git

# Install Java openjdk-8 to enable py4jps
# (See https://github.com/puckel/docker-airflow/issues/182#issuecomment-594906148)
USER root
RUN echo "deb http://security.debian.org/debian-security stretch/updates main" >> /etc/apt/sources.list
RUN mkdir -p /usr/share/man/man1 && \
    apt-get update -y && \
    apt-get install -y openjdk-8-jdk

# Required to install, otherwise will thorw below error when install Mono:
# #8 0.390 E: gnupg, gnupg2 and gnupg1 do not seem to be installed, but one of them is required for this operation
RUN apt-get update && apt-get install -y gnupg

# Mono and pythonnet install bit taken from:
# https://github.com/jonemo/pythonnet-docker/blob/master/archived-dockerfiles/python3.7.7-mono5.20-pythonnet2.5.0
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

# TODO - delete this once new version of pyderivationagent is released
# TODO - update requirements.txt to use new version of pyderivationagent
# RUN python -m pip install git+https://github.com/cambridge-cares/TheWorldAvatar@161-dev-pyderivationagent-modifications#subdirectory=JPS_BASE_LIB/python_derivation_agent
RUN python -m pip install --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple pyderivationagent==1.1.0b0
RUN python -m pip install git+https://github.com/cambridge-cares/TheWorldAvatar@134-dev-lab-equipment-digital-twin#subdirectory=Agents/utils/chemistry-and-robots


# # # Re-install the version of JPS_BASE_LIB that is been developing
# # # (sinse the newly added code is not in the release version of py4jps)
# # # TO BE REMOVED WHEN MERGE TO DEVELOP
# # RUN jpsrm uninstall JpsBaseLib
# # RUN mkdir /jpstemp
# # COPY jps-base-lib.jar ./jpstemp/jps-base-lib.jar
# # COPY lib ./jpstemp/lib
# # RUN jpsrm install JpsBaseLib ./jpstemp/

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

COPY FCRemote.dll /app/FCRemote.dll
COPY FCRemoteCSV.dll /app/FCRemoteCSV.dll

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'python_agent' module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
USER appuser
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "vapourtecagent:create_app()"]
