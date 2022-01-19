# Email Agent

## Purpose
This EmailAgent uses the JPS Asynchronous Watcher framework and listens for incoming HTTP requests. Once a valid HTTP request has been received (and the originating source is approved), then the contents are forwarded onto an SMTP server (of the administrator's choosing) to be dispatched as an email. A class (EmailSender) has been provided within the JPS Base Library to facilitate this. If the EmailAgent cannot be reached (or the request is not approved), then the contents are written to a local log file instead.
 
Once running in a central location, this agent will allow all other developers of JPS services to send automated emails containing error reports, availablity statuses, or analytics, without each service needing to include its own copy of the SMTP server's credentials.

## Calling the Email Agent
To make use of the Email Agent from your own code, follow the below steps:

1. Ensure you're using version `1.4.0-SNAPSHOT` (or above) of the JPS Base Library
2. Set the value of the `EMAIL_AGENT_URL` environment variable to the location of the EmailAgent running at CMCL (can be provided by CMCL).
3. Use the `sendEmail(subject, body)` method within the JPS Base Library to generate a notification email.

**Note:** The EmailAgent is currently configured to only approve requests if they've originated from the same network (or from CMCL's public IP). This has been done so that the service isn't flooded with emails when developers are running/testing their code locally. In that situation, the EmailAgent will deny the request and the EmailSender class within the JPS Base Library will write the content to a local log file instead.

### Acceptable Use
Please note that it is not the intention for this EmailAgent to be used to send regular debugging information, or replace the use of local logging/unit tests. It is envisioned that when a JPS service encounters an error **_during_** its lifecycle (e.g. no longer being able to access a KG endpoint, or external service for data acquisition), a single email should be send to alert developers that something is wrong; developers can then investigate the offending service to view log information and stack traces. Errors that can be detected at build time (e.g. missing libraries, presence of required files) should be written as unit tests.


## Building the Image
The `docker` folder contains the required files to build a Docker Image for the EmailAgent service; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
* The `docker build` command should be run from the `EmailAgent` directory; this is so that a copy of the `email-agent-code` directory can be copied into the final Image.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.

### Configuring Maven
The Java code that this Image is setup to build needs access to [TheWorldAvatar Maven Repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). To allow access to this repository, developers will need to provide their credentials in single-word text files at the following locations:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

The `repo_username.txt` file should contain your GitHub username, and the `repo_password.txt` file your GitHub [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that permits [publishing and installing packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages). These files **__must__** not be committed to the repository.

### Setting Properties
This agent relies on the existance of a properties file that contains settings and credentials used to contact the SMTP server. Each developer is expected to create a new properties file with their settings and credentials before attempting to build the Image; this can be based on the example file within the `email-agent-code/data` folder. Note, it is important that the properties file with these credentials is not committed to the GitHub repository.

For the agent code to find the properties file, an environment variable named `EMAIL_AGENT_PROPERTIES` will be created with the location of the file. For the existing Docker configuration to pick up the developer's properties file, copy it into the Image, and generate the appropriate environment variable, place the properties file at the following location:
```
./email-agent-code/data/email-agent.properties
```

### Docker Commands
Once the requirements have been addressed, the Image can be built using the following methods. Note that once this code has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the software (which is listed within the `pom.xml` file). For more information on versioning, refer to the Wiki.

+ To pull the Image:
  + `docker-compose -f ./docker/docker-compose.yml pull`
+ To build the Image:
  + `docker-compose -f ./docker/docker-compose.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate`

For experienced Docker developers, a bash script has been provided (`quick-build.sh`) that allows all stages of the build to be cached and used to speed up future builds.
