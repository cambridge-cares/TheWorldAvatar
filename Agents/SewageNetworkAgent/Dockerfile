# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory, credentials and config files
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the latter, but not the former
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/SewerageNetworkAgent/
RUN --mount=type=cache,target=/root/.m2/repository mvn package


#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

# Copies all files in config directory to destination
COPY ./SewageNetworkAgent/src/test/resources. /root/

# Set the required environment variables
ENV HGDATA0="/root/HG_instances_0.csv"
ENV HGDATA1="/root/HG_instances_1.csv"
ENV HGDATA2="/root/HG_instances_2.csv"
ENV HGDATA3="/root/HG_instances_3.csv"
ENV HGDATA4="/root/HG_instances_4.csv"
ENV HGDATA5="/root/HG_instances_5.csv"
ENV HGDATA6="/root/HG_instances_6.csv"
ENV KGDATA0="/root/KG_instances_0.csv"
ENV KGDATA1="/root/KG_instances_1.csv"
ENV KGDATA2="/root/KG_instances_2.csv"
ENV KGDATA3="/root/KG_instances_3.csv"
ENV KGDATA4="/root/KG_instances_4.csv"
ENV KGDATA5="/root/KG_instances_5.csv"
ENV KGDATA6="/root/KG_instances_6.csv"
ENV KGDATA7="/root/KG_instances_7.csv"
ENV KGDATA8="/root/KG_instances_8.csv"
ENV KGDATA9="/root/KG_instances_9.csv"
ENV KGDATA10="/root/KG_instances_10.csv"
ENV KGMainNet="/root/KGMainNetwork.csv"
ENV KGSubNet="/root/KGSubNetwork.csv"
ENV Branch0="/root/HG_main_sub_instances_final_0.csv"
ENV Branch1="/root/HG_main_sub_instances_final_1.csv"
ENV Branch2="/root/HG_main_sub_instances_final_2.csv"
ENV Branch3="/root/HG_main_sub_instances_final_3.csv"

COPY --from=builder /root/SewerageNetworkAgent/output/seweragenetwork-agent##1.0.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================
