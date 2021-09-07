# Status Agent

## Purpose
The StatusAgent provides a number of availability tests to check that KG endpoints (and other agents) are available at their expected URLs, and contain the expected data. These tests are run on a regular schedule, and the results viewed via a provided webpage. Manual execution of the tests can also be triggered via the webpage and HTTP requests.

Once running in a central location, this agent (and it's associated webpage) will allow developers and users of TheWorldAvatar project to check that the expected KGs namespaces are accessible at the expected locations.

## Accessing the Status Agent

To access the Status Agent's webpage, simply visit the `/dashboard` URL of the server the agent is running on. 

## Building the Image
The `docker` folder contains the required files to build a Docker Image for the StatusAgent service; the `Dockerfile` file contains the instructions to build an Image; before making any changes to it, please consult the application's developer or the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure it is the correct one.
* The `docker build` command should be run from the `StatusAgent` directory; this is so that a copy of the `status-agent-code` directory can be copied into the final Image.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.

### Configuring Maven
The Java code that this Image is setup to build needs access to [TheWorldAvatar Maven Repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). To allow access to this repository, developers will need to provide their credentials in single-word text files at the following locations:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

The `repo_username.txt` file should contain your GitHub username, and the `repo_password.txt` file your GitHub [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that permits [publishing and installing packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages). These files **__must__** not be committed to the repository.

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
