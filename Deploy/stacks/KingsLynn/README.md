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
# Spinning up the Stack

This section explains how to spin up the core stack and upload initial data sets, i.e. high-resolution population raster data and (optionally) OntoCityGml building instances.
If using VSCode, all required VSCode extensions shall be installed (on the remote machine if applicable) for all convenience scripts to work properly, i.e. *augustocdias.tasks-shell-input*.

<span style="color:red">
The functionality has been tested based on commit `3723a574c6685279a70b814b43df6b4027d5c305` on branch `272-ability-to-upload-triples-through-the-stack-data-uploader`.
</span>

&nbsp;
## Spinning up the core Stack

Navigate to `Deploy/stacks/dynamic/stack-manager` and run the following command there from a *bash* terminal. To [spin up the stack], both a `postgis_password` and `geoserver_password` file need to be created in the `stack-manager/inputs/secrets/` directory (see detailed guidance following the provided link). There are several [common stack scripts] provided to manage the stack:

```bash
# Start the stack (please note that this might take some time)
bash ./stack.sh start KINGS-LYNN

# Stop the stack
bash ./stack.sh stop KINGS-LYNN

# Remove stack services (incl. volumes)
bash ./stack.sh remove KINGS-LYNN -v
```

After spinning up the stack, the GUI endpoints to the running containers can be accessed via Browser (i.e. adminer, blazegraph, ontop, geoserver). The endpoints and required log-in settings can be found in the [spin up the stack] readme.

&nbsp;
## Spinning up the core Stack remotely via SSH

To spin up the stack remotely via SSH, VSCode's in-built SSH support can be used. Simply follow the steps provided here to use [VSCode via SSH] to log in to a remote machine (e.g. Virtual machine running on Digital Ocean) an start deployment. Regular log in relies on username and password. To avoid recurring prompts to provide credentials, one can [Create SSH key] and [Upload SSH key] to the remote machine to allow for automatic authentification.

Once logged in, a remote copy of The World Avatar repository can be cloned using the following commands:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git <REPO NAME>
$ cd <REPO NAME>
$ git checkout dev-MetOfficeAgent-withinStack
$ git pull
```
Once the repository clone is obtained, please follow these instructions above to spin up the stack on the remote machine. In order to access the exposed endpoints, e.g. `http://localhost:3838/blazegraph/ui`, please note that the respective ports might potentially be opened on the remote machine first.

To prevent and identify potential permission issues on Linux machines (i.e. for executable permission), the following commands can be used to verify and manage permissions:

```bash
# Check permissions
ls -l <REPO NAME>
# Grant permissions
chmod -R +rwx <REPO NAME>
```

&nbsp;
## Uploading initial data

<span style="color:red">
To be revised/checked, incl. links in data readmes
</span>

A few datasets/files to be uploaded to the stack are provided in the `inputs` folder of this repository. Uploading pre-instantiated OntoCityGml quads is optional but highly recommended to skip steps 1 - 4.2 (depending on the exact quads file provided) of the building instantiation workflow below.
The following steps explain how to upload the data to the stack:

1) Copy all relevant files from the `inputs` folder of this repository into the `inputs` folder of  the stack data uploader repository:

    a) Copy the configuration files from the `inputs/configs/` directory to the matching directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/configs/`

    b) Replace the `readme.txt` files in the `inputs/data/*` sub-folders with the referenced data files from `../../Data/...`

    c) Copy all data sub-directories from the `inputs/data` directory into the matching parent directory in `Deploy/stacks/dynamic/stack-data-uploader/inputs/data/`

2) Create a quad- and geospatially-enabled Blazegraph namespace `ocgml` via the Blazegraph GUI, i.e. http://128.199.197.40:3838/blazegraph/ui/#namespaces

3) Navigate to `Deploy/stacks/dynamic/stack-data-uploader` and run the following command there from a *bash* terminal and wait until container has stopped again (i.e. the upload has finished):
    ```bash
    bash ./stack.sh start KINGS-LYNN
    ```


&nbsp;
# Instantiation workflow (for building data)

## <u>1) Geospatial data consolidation (QGIS)</u>

## <u>2) Creation of .gml input file for CKG import (FME)</u>

Re-instantiated King’s Lynn buildings using latest CKG develop branch
                - Encountered issues using ImportAgent, which now seems unable to handle large gml files (which is one of the actual use cases for it)
                - Work around: Use the ImportAgent only to split the large gml file and upload them manually in chunks via the Importer GUI of 100-200 files

## <u>3) Importing building data into CKG (CKG Importer)</u>

### <u>1) Import Agent (CKG)</u>

### <u>2) Import GUI (CKG)</u>

## <u>4) Building data enrichment</u>

### <u>1) Thematic Surface Discovery Agent (CKG)</u>

Set up AccessAgent locally
                - TSD works smoothly on entire namespace
                - UPRN agent seems unable to handle entire namespace, i.e. large amounts of buildings, due to heap space issues (even on Mehal’s 64GB RAM machine)
                - Workaround: Python script to recurringly call agent for individual buildings


### <u>2) UPRN Agent (in chunks)</u>

### <u>3) Energy Performance Certificate Agent</u>

### <u>4) Building Matching Agent</u>

### <u>5) Property Sales Instantiation Agent</u>

## <u>5) Additional data incorporation </u>

### <u>MetOffice Agent</u>

### <u>AirQuality Agent</u>

### <u>River Levels Agent</u>


<!-- Links -->
[Container registry on Github]: https://github.com/orgs/cambridge-cares/packages
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
[allows you to publish and install packages]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages
[personal access token]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token
[spin up the stack]: https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md
[common stack scripts]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/common-scripts
[VSCode via SSH]: https://code.visualstudio.com/docs/remote/ssh
[Create SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/create-with-openssh/
[Upload SSH key]: https://docs.digitalocean.com/products/droplets/how-to/add-ssh-keys/to-existing-droplet/

<!-- Agents -->
[UPRN Agent in batches]: https://github.com/markushofmeister/KingsLynnUtils
