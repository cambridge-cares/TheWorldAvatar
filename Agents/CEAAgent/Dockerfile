#==================================================================================================
# First stage: build cea
#==================================================================================================
FROM cityenergyanalyst/cea:v3.39.2 as cea

#==================================================================================================
# Second stage: build jar file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/cea-agent
RUN --mount=type=cache,target=/root/.m2/repository mvn package -Dmaven.test.skip=true

#==================================================================================================
# Third stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM mcr.microsoft.com/openjdk/jdk:11-ubuntu as agent

# Copy the Python environment from the `cea` image (full Python environment)
COPY --from=cea /opt/conda /opt/conda

# Add the Python environment to the system PATH
ENV PATH="/opt/conda/bin:$PATH"
ENV PYTHONPATH="/opt/conda/lib/python3.8/site-packages:$PYTHONPATH"

# Copy the jar file built in the builder stage
COPY --from=builder /root/cea-agent/target/ ./target/

# Ensure required libraries are installed (libgl1 for pythonOCC)
RUN apt-get update && apt-get install -y libgl1

# Bugfix for matplotlib, see here: https://stackoverflow.com/questions/37604289/tkinter-tclerror-no-display-name-and-no-display-environment-variable
RUN mkdir -p ~/.config/matplotlib && echo "backend: Agg" > ~/.config/matplotlib/matplotlibrc

# Install necessary Python packages (rasterio, fiona) in the Python environment
RUN /opt/conda/bin/python3 -m pip install rasterio fiona pysal

# When image is run, run the code with the environment activated:
SHELL ["/bin/bash", "-c"]

# Start the Tomcat server
ENTRYPOINT ["java", "-cp", "target/cea-agent-3.6.0-jar-with-dependencies.jar", "uk.ac.cam.cares.jps.agent.cea.Main"]
#==================================================================================================