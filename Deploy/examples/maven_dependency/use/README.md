# Example: Building an image with Maven dependencies

## What this image is for

The purpose of this image is to demonstrate how [Maven](https://maven.apache.org) can be used to handle dependencies when building a service (agent, web interface, datastore etc.).
It should be useful as a template if the application you're deploying has dependencies that aren't suitable for managing with git, e.g. binary training data for an agent.
For Java applications where *all* of the dependencies are managed with Maven, the [Java agent template](../../java_agent/README.md) might be a more suitable starting point.

The dependency is assumed to consist of one or more target files stored in a single Maven artifact, but it should be easy enough to adapt this example if you have multiple artifacts.

## How it works

The image uses a [multi-stage build](https://docs.docker.com/develop/develop-images/multistage-build) to:

  1. Download an artifact from a Maven repository
  2. Copy the artifact into a second image that can be built into a standalone service

The advantages of this approach are:
  * The final image doesn't include superfluous files required only for the dependency download
  * Credentials can be supplied to Maven easily, with no risk of exposing them in the final image

## Using the template

The URL of the Maven repository and the details (group, ID, type, version) of your artifact should be set using the 'ARG' directives in the Dockerfile.
You'll also need to add the username and password for your chosen Maven repository in single-line text files located like this:

```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```

If you're using the repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/, repo_username.txt should contain your github username, and repo_password.txt should contain your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages)

By default, the example is set to download a zip file containing two files from the WorldAvatar Maven registry.

To see this working, spin up the container using docker-compose.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The image has a dummy entrypoint that will keep the final container open for you to see the downloaded files.
Start a shell by right-clicking the container in VS Code and choosing 'Attach Shell', or on the command line:

```
docker exec -it app_with_maven_dependency bash
```

In the container, the dependency files will have been unzipped to a directory with the same name as the Maven artifact.
By default, this is 'model_files':

```
ls -l model_files
```

## Adapting the template

After you've set the 'ARG' directives to download your chosen artifact, you can modify the second stage in the Dockerfile to set up the rest of your app/service.

That involves all the usual steps for deploying an application with Docker, which will probably include: choosing an appropriate base image, building your code from source and substituting the dummy entrypoint for one that runs your app or service.