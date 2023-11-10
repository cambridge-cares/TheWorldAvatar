# First stage: build war file
#==================================================================================================
FROM maven:3.6.3-openjdk-17-slim AS builder

# Copy in Maven settings templates and credentials
ADD . /root

# Populate settings templates with credentials, repo name
WORKDIR /root/.m2
# (Note that | rather than / is used as the sed delimiter, since encrypted passwords can contain the former, but not the latter
RUN sed -i "s|MASTER_PASSWORD|$(mvn --encrypt-master-password master_password)|" settings-security.xml
RUN sed -i "s|WA_REPO_USERNAME|$(cat ../credentials/wa_repo_username.txt)|;s|WA_REPO_PASSWORD|$(cat ../credentials/wa_repo_password.txt|xargs mvn --encrypt-password)|" settings.xml

# Build
WORKDIR /root/filter-agent
RUN --mount=type=cache,target=/root/.m2/repository mvn clean package -U

#==================================================================================================

# Second stage: build war file
#==================================================================================================

FROM maven:3.6.3-openjdk-17-slim AS agent

WORKDIR /usr/local/filter-agent
COPY --from=builder /root/filter-agent/target/*.jar app.jar

ENV JAVA_OPTS=${JAVA_OPTS}

COPY ./entrypoint.sh ./entrypoint.sh

ENTRYPOINT ["./entrypoint.sh"]