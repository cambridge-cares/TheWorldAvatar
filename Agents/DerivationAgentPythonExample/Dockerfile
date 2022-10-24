# This Dockerfile has been adapted from the VS Code example at https://code.visualstudio.com/docs/containers/quickstart-python

# Base image is a lightweight version of Python
FROM python:3.8-slim-buster

# Meta data
# NOTE change the label to your email and description
LABEL authors = "jb2197@cam.ac.uk"
LABEL description = "Minimum working example for developing derivation agent using pyderivationagent package."

# Install required software: git and openjdk-11
USER root
RUN apt update && apt install -y git openjdk-11-jdk-headless

# Expose the port on which our server will run
EXPOSE 5000

# Turns off buffering for easier container logging
ENV PYTHONUNBUFFERED=1

# Install the required Python libraries
COPY requirements.txt .
RUN python -m pip install -r requirements.txt

# # Re-install the version of JPS_BASE_LIB that is been developing
# # (this will be required if the new features are not merged back to main)
# RUN jpsrm uninstall JpsBaseLib
# RUN mkdir /jpstemp
# COPY /_temp/jps-base-lib.jar ./jpstemp/jps-base-lib.jar
# COPY /_temp/lib ./jpstemp/lib
# RUN jpsrm install JpsBaseLib ./jpstemp/

# Set the default working directory, then copy the Python source code into it
# NOTE change the added folder to the name of your project
WORKDIR /app
ADD derivationagentpythonexample /app/derivationagentpythonexample

# Switch to a non-root user before running the server, for security reasons
# (See https://code.visualstudio.com/docs/containers/python-user-rights)
RUN useradd appuser && chown -R appuser /app

# Creating the user doesn't create their home directory for some reason, so create it now
RUN mkdir /home/appuser
RUN chown -R appuser:appuser /home/appuser
RUN chmod -R 755 /home/appuser

# Start the gunicorn server on port 5000, using a Flask object called 'app' imported from the 'derivationagentpythonexample' module
# Note that port 5000 is *inside the container*; this can be mapped to a port on the host when running the container on the command line or in docker-compose.yml
# NOTE change the name of the module to the name of your project
USER appuser
ENTRYPOINT ["gunicorn", "--bind", "0.0.0.0:5000", "derivationagentpythonexample:create_app()"]
