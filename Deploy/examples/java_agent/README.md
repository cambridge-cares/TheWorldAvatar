# Example: Java agent

## What this image is for

The purpose of this image is to show how Docker can be used to deploy an agent as a Java servlet running in a Tomcat server.
It should be useful as template if your servlet extends the uk.ac.cam.cares.jps.base.agent.JPSAgent class.

## How it works

The image uses a [multi-stage build](https://docs.docker.com/develop/develop-images/multistage-build) to:

  1. Generate a .war file from some Java code with Maven dependencies.
  2. Deploy the .war file on a Tomcat server.

The advantages of this two-stage approach are:
  * The final image doesn't include files that aren't needed at runtime, i.e. those required only for building the .war file.
  * Credentials can be supplied to Maven easily, with no risk of exposing them in the final image.

## Using the template

The template is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, you simply need to spin up a container from the image.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The agent is reachable on localhost port 8081 by default (you can change this in docker-compose.yml).
To test it, you can you use 'curl' to send it a request on the command line (taking care to escape the ampersand in the argument list):

```
result=$(curl -s http://localhost:8081/java_agent_eg/api/v1?a=2\&b=3) && echo $result
```

Here, 'a' and 'b' are the two variables to be summed; the result is returned in a third variable, 'c', in json format:

```
{"c":[5]}
```

## Adapting the template

To deploy your agent:

* Make a copy of this whole directory
* Replace java_agent/pom.xml and the contents of java_agent/src and java_agent/WEB-INF with your own files

Note that, if you rename the 'java_agent' subdirectory, you'll need to change the 'COPY' command in the second build stage of the Dockerfile.
Similarly, if your pom.xml generates the .war file in a location other than ./output/, the same command will need to be updated.