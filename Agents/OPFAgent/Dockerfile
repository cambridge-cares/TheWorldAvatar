# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/opf-agent
RUN --mount=type=cache,target=/root/.m2/repository mvn package
#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

COPY --from=builder /root/opf-agent/target/opf-agent##1.0.0-SNAPSHOT.war $CATALINA_HOME/webapps/

# Install Python
RUN apt-get update
RUN apt-get install -y python3

# Install pip and scipy, pypower packages
RUN curl https://bootstrap.pypa.io/get-pip.py --output get-pip.py
RUN python3 get-pip.py
RUN pip install scipy
RUN pip install pypower

# Replace outdated files in pypower
COPY --from=builder /root/opf-agent/python/pypower/dSbr_dV.py /usr/local/lib/python3.10/dist-packages/pypower
COPY --from=builder /root/opf-agent/python/pypower/gausspf.py /usr/local/lib/python3.10/dist-packages/pypower

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================