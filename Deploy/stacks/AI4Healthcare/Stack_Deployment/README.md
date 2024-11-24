# AI for Public Health

Key sections:
- [1. Prerequisites](#1-prerequisites): Preparations required before spinning up the use case stack
- [2. Spinning up the Stack](#2-spinning-up-the-stack): How to spin up the core stack and upload initial data sets
- [3. Data instantiation workflow](#3-data-instantiation-workflow): How to deploy all required agents (sequence, interdependencies, etc.)
- [5. Triggering new derivation cascades](#5-triggering-new-derivation-cascades): How to manually trigger new derivation cascades (mainly for showcase purposes)
- [6. Redeployment](#6-redeployment): How to restart stack and agents (after initial data instantiation workflow)
- [7. Incorporate CReDo network visualisation](#7-connecting-with-credo-visualisation): How to incorporate synthetic network data from CReDo into the visualisation
- [Potential refinements/next steps](#potential-refinementsnext-steps): Potential refinements for future work


&nbsp;
# 1. Prerequisites

## Access to Docker registries

Spinning up the Docker stack requires access to the [Container registry on Github] to pull (agent) images. Access needs to be ensured beforehand via your Github [personal access token], which must have a `scope` that [allows you to publish and install packages].

To log in to the container registry, please run the following command to establish the connections and provide your access token when prompted. For more details please refer to the linked resources.
```bash
# Github Container registry
$ docker login ghcr.io -u <github_username>
$ <github_personal_access_token>
```

&nbsp;
# 2. Spinning up the Stack




<!-- Links -->
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Environment Agency]: https://environment.data.gov.uk/flood-monitoring/doc/reference
[forwarding the port]: https://code.visualstudio.com/docs/remote/ssh#_forwarding-a-port-creating-ssh-tunnel
[OS Features API]: https://api.os.uk/features/
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/
[MetOffice My Account]: https://register.metoffice.gov.uk/MyAccountClient/account/view

<!-- Stack references -->
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[Stack data uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[Stack manager]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md

<!-- Agents -->
[AccessAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AccessAgent
[Property Value Estimation Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent/README.md
[Property Sales Instantiation Agent resources folder]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HMLandRegistryAgent/resources
[MetOffice Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/MetOfficeAgent
[AirQuality Agent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/AirQualityAgent

<!-- Repositories -->
[data.json]: /StackDeployment/inputs/stack-manager/inputs/data/visualisation/data.json
[Agent docker-compose file folder]: /StackDeployment/inputs/docker_compose_files

<!-- Files -->
[routing.json]: /StackDeployment/inputs/access_agent/routing.json
[CKG config.properties]: https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/src/main/resources/config.properties