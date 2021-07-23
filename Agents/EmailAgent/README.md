# Email Agent

## Purpose
This EmailAgent uses the JPS Asynchronous Watcher framework and listens for incoming HTTP requests. Once a valid HTTP request has been recieved (and the originating source is approved), then the contents are forward onto an SMTP server (of the developer's choosing) to be dispatched as an email. A class (EmailSender) has been provided within the JPS Base Library to facilitate this. If the EmailAgent cannot be reached (or the request is not approved), then the contents are written to a local log file instead.
 
Once running in a central location, this agent will allow all other developers of JPS services to send automated emails containing error reports, availablity statuses, or analytics, without each service needing to include it's own copy of the SMTP server's credentials.

### Acceptable Use
Please note that it is not the intention for this EmailAgent to be used to send regular debugging information, or replace the use of local logging. It is envisioned that when a JPS service encounters a fatal error (for example, no longer being able to access a KG endpoint, or external service for data acquisition), a single email should be send to alert developers that something is wrong. Developers can then investigate the offending service to view log information and stack traces.


## Building the Image
The `docker` folder contains the required files to build a Docker image for the EmailAgent service; the `Dockerfile` file contains the instructions to build an image; before making any changes to it, please consult the application's developer the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure this is the right one.
* The `docker build` command should be run from the `EmailAgent` directory (not this one); this is so that a copy of the `email-agent-code` directory can be copied into the image.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.

### Configuring Maven
The Java code that this image is setup to build needs access to [TheWorldAvatar maven repository ](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). To allow access to this repository, developers will need to provide their credentials in single-word text files located at the following locations:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

The `repo_username.txt` file should contain your Github username, and the `repo_password.txt` file your Github [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that permits [publishing and installing packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages). these files must not be committed to the repository.

### Setting Properties
This agent relies on the existance of a properties file that contains settings and credentials used to contact the SMTP server. Each developer is expected to create a new properties file with their settings and credentials before attempting to build the image; this can be based on the example file within the `email-agent-code/data` folder. Note, it is important that the properties file with these credentials is not committed to any Git repository.

For the agent code to find the properties file, an environment variable named `EMAIL_AGENT_PROPERTIES` will be created with the location of the file. For the existing Docker configuration to pick up the developer's properties file, copy it into the Image, and generate the appropriate environment variable, place the properties file at the following location:
```
./email-agent-code/data/email-agent.properties
```

### Docker Commands
Once the requirements have been addressed, the Image can be build using the following methods. Note that once this code has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the software (which is listed within the `pom.xml` file). For more information on versioning, refer to the Wiki.

+ To build the image:
  + `docker build --rm --no-cache -t email-agent:1.0.0-SNAPSHOT -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d -p 8099:8080 --restart always --name "email-agent" -it email-agent:1.0.0-SNAPSHOT`

  Alternatively, the provided `docker-compose.yml` file can be used to spin up a stack containing only the EmailAgent. This can be done using the `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate` command (adding `--build` if the Image needs to be rebuilt before starting the stack).