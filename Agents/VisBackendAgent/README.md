# Vis Backend Agent

The Vis-Backend Agent is a supporting service to The World Avatar's [visualisation platform (ViP)](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-platform). It is designed to manage all visualisation-related requests from a single point of access to for example, filter map layers or generate dynamic controls. By abstracting the backend implementation details (such as which other agents to call), it provides a unified access point to the data within its specific stack. This design allows the ViP to be deployed on a separate stack while retaining the capability to ingest data from multiple stacks seamlessly.

## Table of Contents

- [Vis Backend Agent](#vis-backend-agent)
  - [1. Agent Deployment](#1-agent-deployment)
    - [1.1 Preparation](#11-preparation)
    - [1.2 Docker Deployment](#12-docker-deployment)

## 1. Agent Deployment

The agent is designed for execution through a Docker container. Other deployment workflows are beyond the scope of this document.

### 1.1 Preparation

Before using this agent, follow the steps below to ensure you have everything you need to successfully run the agent.

##### Maven Repository credentials

This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide your credentials in a single-word text files located like this:

```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

### 1.2 Docker Deployment

**TEST ENVIRONMENT**

- Deploy the agent to execute the unit tests on a standalone container by running the following code in the CLI at the <root> directory.
- The success of all tests must be verified through the Docker logs.

```
docker compose -f "./docker/docker-compose.test.yml" up -d --build
```

**PRODUCTION ENVIRONMENT**

- Deploy the agent on a standalone container by running the following code in the CLI at the <root> directory.

```
docker compose -f "./docker/docker-compose.yml" up -d
```
