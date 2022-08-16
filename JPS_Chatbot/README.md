## Deploying the JPS Chatbot with Docker

The "Dockerfile" in this directory contains instructions to build a Docker image for the JPS Chatbot.
Before making any changes to it, please consult the application's developer (Xiaochi Zhou <xz378@cam.ac.uk>) and the system administrators at CMCL (Michael Hillman <mdhillman@cmclinnovations.com>).

### In a Docker stack

The JPS Chatbot (and it's associated LDF server) have been added to the 'agent' Docker stack (see the [deployment readme](../Deploy/README.md) for more info).

### In isolation (for development and local testing)
The instructions below are intended for isolated development and testing only.

1. Create a file at ./docker/credentials/repo_username.txt and add your github username.
2. Create a file at ./docker/credentials/repo_password.txt and add your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token). The token must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).
3. Ensure localhost port 55001 is not already in use
4. On the command line, from the `/Deploy/stacks` directory, run the `./build-stack.sh agent dev jps-chabot` to build the image (without the rest of the agent stack) in development node.
5. To run the image in isolation, from the same directory as above, run the `./start-stack.sh agent dev jps-chabot`.

The chatbot can also be built/run as part of the wider agent stack (for integration testing) by omitting the `jps-chabot` argument at the end of the previous commands.
