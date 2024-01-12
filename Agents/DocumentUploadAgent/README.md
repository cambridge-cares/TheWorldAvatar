
# DocumentUpload Agent
An agent designed to save and instantiate documents int a KG.


## Installation

### Agent
Update the following files in the `./config` folder:
- `/agent.properties`:
    - `endpoint.kg.[NAMESPACE]`: The SPARQL endpoint of each respective NAMEPSACE.
    - `auth.kg.user` and `auth.kg.pass`: Blazegraph authentication. Can be left blank if no authentication is needed.
    - `endpoint.printer`: Printer server endpoint.
    - `target_qr_size`: QR code size target in cm.
    - `url.manual`: URL for the accessing the asset's manual. The URL produced will have the manual name appended to the end of the URL.

- `/ontologyMap.properties`: A map of the asset class and their respective ontology. Please update if there is any new asset class or change in ontology structure.

Update the following files in the `./credentials` folder:
 - Add your git credentials with your git username and token in respectively `repo_username.txt` and `repo_password.txt`.

Update `./docker-compose.yml`:
 - Volumes: Change the bind mount folder location to your need for the agent to store the manuals. In case the Stack is used, this may need to be changed

#### For use in The Stack
Update  `./stack-manager-input-config-service/document-upload-agent.json`:
 - Update the bind mount to where the manuals are to be stored


The agent could be built using the following command:
```bash
docker-compose up -d
```



    
## Features
