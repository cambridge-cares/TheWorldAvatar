# Email Agent

## Purpose
This EmailAgent uses the TWA Asynchronous Watcher framework and listens for incoming HTTP requests. Once a valid HTTP request has been received (and the originating source is approved), then the contents are forwarded onto an SMTP server (of the administrator's choosing) to be dispatched as an email.

A class (EmailSender) has been provided within the JPS Base Library to facilitate this. If the EmailAgent cannot be reached (or the request is not approved), then the contents are written to a local, client-side log file instead.
 
Once running in a central location, this agent will allow other developers of TWA services to send automated emails containing error reports, availablity statuses, or analytics, without each service needing to include its own copy of the SMTP server's credentials.


## Acceptable Use
Please note that it is not the intention for this EmailAgent to be used to send regular debugging information, or replace the use of local logging/unit tests. It is envisioned that when a TWA service encounters a major error **_during_** its lifecycle (e.g. no longer being able to access a KG endpoint, or external service for data acquisition), a single email should be send to alert developers that something is wrong; developers can then investigate the offending service to view log information and stack traces. Errors that can be detected at build time (e.g. missing libraries, presence of required files) should be written as unit tests.


## Building the Image
The agent directory contains the required files to build a Docker Image for the EmailAgent service; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL. Files have also been added to ensure the agent is compatable for deplyment in a [stack environment](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
* The `docker build` command should be run from the `EmailAgent` directory.

Note: users are free to use the uploaded copy of the image form the TWA image registry, as the properties file containing SMTP server details should be stored in the `./code/data` folder, which is mounted to the image as a volume at runtime.

### Configuring Maven
The Java code that this image is setup to build needs access to [TheWorldAvatar Maven Repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). To allow access to this repository, developers will need to provide their GitHub credentials in single-line text files at the following locations:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

The `repo_username.txt` file should contain your GitHub username, and the `repo_password.txt` file your GitHub [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that permits [publishing and installing packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages). These files **__must__** not be committed to the repository.


### Setting Properties
This agent relies on the existence of a properties file that contains settings and credentials used to contact the SMTP server. Each developer is expected to create a new properties file with their settings and credentials before attempting to build the Image; this can be based on the example file within the `./code/data` folder. Note, it is important that the properties file with these credentials is not committed to the GitHub repository.

For the agent code to find the properties file, an environment variable named `EMAIL_AGENT_PROPERTIES` will be created with the location of the file. For the existing Docker configuration to pick up the developer's properties file, simply create your properties file at the following location before deplying the image:

```
./code/data/email-agent.properties
```

### Docker Commands
Once the requirements have been addressed, the Image can be built using the following methods. Note that the agent can also be run as part of a stack using the regular methods (see stack documentation).

+ To pull the Image:
  + `docker-compose -f docker-compose.yml -pull`
+ To build the Image:
  + `docker-compose -f docker-compose.yml -f docker-compose-build.yml build --force-rm`
+ To generate a Container (i.e. run the Image):
  + `docker-compose -f docker-compose.yml up -d --force-recreate`


## Calling the Email Agent
To make use of the Email Agent from your own code, follow the below steps:

- Ensure you're using the latest version of the JPS base library.
- Set the value of the `EMAIL_AGENT_URL` environment variable to the base URL of the remote EmailAgent.
  - This should be the base URL of the agent, without a route `e.g. http://my-website.com/email_agent`
- Use the `sendEmail(subject, body)` method of the `EmailSender` class in JPS Base Library to generate a notification email.

For an example of this, or to quickly test that your remote EmailAgent is up and running, see the `SubmissionTest` class within the EmailAgent.