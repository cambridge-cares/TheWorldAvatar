# First stage: build war file
#==================================================================================================
FROM maven:3.8.6-eclipse-temurin-11-focal as builder

# Copy relevant files for building into root's home, including the source, pom file, ./m2 directory, and credentials
ADD /Ifc2OntoBIM /root/Ifc2OntoBIM
ADD /credentials /root/credentials
ADD /.m2 /root/.m2

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build the war
WORKDIR /root/Ifc2OntoBIM
RUN --mount=type=cache,target=/root/.m2/repository mvn clean package

#==================================================================================================

# Second stage: copy the jar file into a new image and run code
#==================================================================================================
FROM tomcat:9.0.68-jre11-temurin as agent

# Copies all files in config directory to destination
COPY --from=builder /root/Ifc2OntoBIM/output/ifc2ontobim-agent##1.1.0.war $CATALINA_HOME/webapps/

# Download dependency
ADD https://github.com/pipauwel/IFCtoRDF/releases/download/IFCtoRDF-0.4/IFCtoRDF-0.4-shaded.jar .

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================
