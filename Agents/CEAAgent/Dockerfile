#==================================================================================================
# First stage: build cea
#==================================================================================================
FROM cityenergyanalyst/cea:latest as cea

#==================================================================================================
# Second stage: build jar file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/cea-agent
RUN --mount=type=cache,target=/root/.m2/repository mvn package

#==================================================================================================
# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM mcr.microsoft.com/openjdk/jdk:11-ubuntu as agent

COPY --from=builder /root/cea-agent/target/ ./target/

# for pythonOCC to work (used by py4design)
RUN apt-get update && apt-get install -y libgl1

COPY --from=cea /venv /venv
COPY --from=cea /venv/Daysim /venv/Daysim

# bugfix for matplotlib, see here: https://stackoverflow.com/questions/37604289/tkinter-tclerror-no-display-name-and-no-display-environment-variable
RUN mkdir -p ~/.config/matplotlib && echo "backend: Agg" > ~/.config/matplotlib/matplotlibrc

# install rasterio for working with TIF files
RUN /venv/bin/python -m pip install rasterio
# install fiona for shapefile
RUN /venv/bin/python -m pip install fiona

RUN /bin/bash -c "source /venv/bin/activate"

# When image is run, run the code with the environment
# activated:
ENV PATH "/venv/bin:/venv/cea/bin:/venv/Daysim:$PATH"
SHELL ["/bin/bash", "-c"]

# Start the Tomcat server
ENTRYPOINT ["java", "-cp", "target/cea-agent-3.4.2-jar-with-dependencies.jar" , "uk.ac.cam.cares.jps.agent.cea.Main"]
#==================================================================================================