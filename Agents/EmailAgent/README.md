# Email Agent

The folder contains the required files to build a Docker image for the EmailAgent service. The "Dockerfile" file contains the instructions to build an image; before making any changes to it, please consult the application's developer the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

Please note the caveats below before attempting to build the service using Docker:

* The service installed within the Docker image will be based on the current commit of this repository, please ensure you're on the right one.
* The `docker build` command should be run from the `EmailAgent` directory (not this one); this is so that a copy of the `email-agent-code` directory can be copied into the image.
* The port shown below has been set so that it doesn't collide with any other services running on the CMCL systems, feel free to change it temporarily for local testing/development.
	
## Building the Image

The Image is set up to download images from the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```

The `repo_username.txt` file should contain your Github username, and the `repo_password.txt` file your Github [Personal Access Token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Once the requirements have been addressed, the Image can be build using the following methods. Note that once this code has been merged to the develop branch, it should be built as part of one of the existing Docker stacks.

Be aware that the VERSION tag should match the current version of the software (which is listed within the 'version' file). For more information, see the readme in the '/Deploy' directory.

+ To build the image:
  + `docker build --rm --no-cache -t email-agent:1.0.0-SNAPSHOT -f docker/Dockerfile .`
+ To generate a container (i.e. run the image):
  + `docker run -d -p 8099:8080 --restart always --name "email-agent" -it email-agent:1.0.0-SNAPSHOT`

  
Alternatively, the provided `docker-compose.yml` file can be used to spin up a stack containing only the EmailAgent. This can be done using the `docker-compose -f ./docker/docker-compose.yml up -d --force-recreate` command (adding `--build` if you wish to rebuild the Image before starting the stack).