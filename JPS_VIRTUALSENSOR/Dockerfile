FROM openjdk:8-jdk-alpine
VOLUME /tmp
ARG JAVA_OPTS
ENV JAVA_OPTS=$JAVA_OPTS
COPY target/AermodAgent-1.0.0-SNAPSHOT.jar jpsvirtualsensor.jar
EXPOSE 8084
EXPOSE 5005
ENTRYPOINT exec java $JAVA_OPTS -jar jpsvirtualsensor.jar
# For Spring-Boot project, use the entrypoint below to reduce Tomcat startup time.
#ENTRYPOINT exec java $JAVA_OPTS -Djava.security.egd=file:/dev/./urandom -jar jpsvirtualsensor.jar
