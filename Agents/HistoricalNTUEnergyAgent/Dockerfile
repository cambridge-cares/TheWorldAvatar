# First stage: build war file
#==================================================================================================
FROM maven:3.6-openjdk-11-slim as builder

# Copy all files into root's home, including the source, pom file, ./m2 directory, credentials and config files
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|REPO_USERNAME|$(cat ../credentials/repo_username.txt)|;s|REPO_PASSWORD|$(cat ../credentials/repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/HistoricalNTUEnergyAgentCode
RUN --mount=type=cache,target=/root/.m2/repository mvn package -DskipTests


#==================================================================================================

# Second stage: copy the downloaded dependency into a new image and build into an app
#==================================================================================================
FROM tomcat:9.0 as agent

WORKDIR /app

# Copy in the properties file
COPY ./config/client.properties /root/client.properties
COPY ./config/agent.properties /root/agent.properties
COPY ./config/xlsxconnector.properties /root/xlsxconnector.properties

# Copy in the mapping folder
COPY ./config/mappings /root/mappings

# Copy in the energy data
COPY ./config/NTU_Energy_Consumption.xlsx /root/NTU_Energy_Consumption.xlsx
COPY ./config/Bus_node_specs.xlsx /root/Bus_node_specs.xlsx
COPY ./config/Branch_specs.xlsx /root/Branch_specs.xlsx
COPY ./config/Generator_specs.xlsx /root/Generator_specs.xlsx
COPY ./config/PV_specs.xlsx /root/PV_specs.xlsx
COPY ./config/venue_info.xlsx /root/venue_info.xlsx
COPY ./config/minimised_class_schedule.xlsx /root/minimised_class_schedule.xlsx


# Set the required environment variables
ENV HISTORICALNTUENERGY_AGENTPROPERTIES="/root/agent.properties"
ENV HISTORICALNTUENERGY_XLSXCONNECTORPROPERTIES="/root/xlsxconnector.properties"
ENV HISTORICALNTUENERGY_CLIENTPROPERTIES="/root/client.properties"
ENV HISTORICALNTUENERGY_AGENT_MAPPINGS="/root/mappings"


# Set the required environment variable
ENV ENERGY_READINGS="/root/NTU_Energy_Consumption.xlsx"
ENV BUSNODE_SPECS="/root/Bus_node_specs.xlsx"
ENV BRANCH_SPECS="/root/Branch_specs.xlsx"
ENV GENERATOR_SPECS="/root/Generator_specs.xlsx"
ENV PV_SPECS="/root/PV_specs.xlsx"
ENV VENUE_INFO="/root/venue_info.xlsx"
ENV CLASS_SCHEDULE="/root/minimised_class_schedule.xlsx"

COPY --from=builder /root/HistoricalNTUEnergyAgentCode/output/historical-ntuenergy-agent##1.0.0.war $CATALINA_HOME/webapps/

# Start the Tomcat server
ENTRYPOINT ["catalina.sh", "run"]
#==================================================================================================