# Kings Lynn Stack

This project contains a step-by-step guide on how to spin up the Stack for the King's Lynn use case and instantiate all  relevant data. It links to other projects and helper scripts where appropriate.

&nbsp;
## Prerequisites

### <u>Access to Docker registries</u>

Spinning up the (core) Docker Stack requires access to the [CMCL Docker Registry] to pull required images. Deploying (pre-built) agents to the spun up Stack requires access to CARES' [Container registry on Github] to pull agent images. Access needs to be ensured beforehand via your Github [personal access token], which must have a `scope` that [allows you to publish and install packages].

To log in to the Container registries, please run the following commands to establish the connections and provide your password/access token when prompted. For more details please refer to the linked resources.
```bash
  # CMCL Container registry
  $ docker login docker.cmclinnovations.com -u <username>
  $ <password>

  # Github Container registry
  $ docker login ghcr.io -u <github_username>
  $ <github_personal_access_token>
```

&nbsp;
## Spinning up the Stack

### <u>Spinning up the core Stack</u>


### <u>Spinning up the Stack remotely via SSH</u>


### <u>Uploading initial data</u>

&nbsp;
## Instantiation workflow (for building data)

### <u>1) Import Agent (CKG)</u>

### <u>2) Thematic Surface Discovery Agent (CKG)</u>

### <u>3) UPRN Agent (in chunks)</u>

### <u>4) Energy Performance Certificate Agent</u>

### <u>5) Building Matching Agent</u>

### <u>6) Property Sales Instantiation Agent</u>


### <u>MetOffice Agent</u>

### <u>AirQuality Agent</u>

### <u>River Levels Agent</u>


<!-- Links -->
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token

<!-- Agents -->
[UPRN Agent in batches]: https://github.com/markushofmeister/KingsLynnUtils
