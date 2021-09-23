## Deploying the JPS Chatbot with Docker

The "Dockerfile" in this directory contains instructions to build a Docker image for the JPS Chatbot.
Before making any changes to it, please consult the application's developer (Xiaochi Zhou <xz378@cam.ac.uk>) and the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>, Owen Parry <oparry@cmclinnovations.com>).

### In a Docker stack
In general, the JPS Chatbot container should be run in the 'web' Docker stack (see the [deployment readme](../Deploy/README.md) for more info).
The instructions below are intended for isolated development and testing only.

### In isolation (for development and local testing)
1. Create a file at ./docker/credentials/repo_host.txt and add your github username.
2. Create a file at ./docker/credentials/repo_password.txt and add your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token). The token must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).
3. Ensure localhost port 55001 is not already in use
4. On the command line, in this directory, run 'docker-compose up' (or right-click docker-compose.yml in VS Code and select 'Compose Up')