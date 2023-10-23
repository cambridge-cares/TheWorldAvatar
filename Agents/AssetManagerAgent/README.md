
# Asset Manager Agent
An agent designed to manage the asset instances in the knowledge graph. The agent is designed to work together with the [Asset Management App](LINK TO APP REPO HERE).

Currently the agent is capable of :
- Instantiating asset data
- Retrieving asset data


## Structure




## Installation
### Printing Server
The target printer need to be installed first in the host of the server. The intallation of this printer may vary between printer types, but generally a printer driver is needed to connect to the printer.Please check with your respective printer manufacturer for the printer driver. Keep in mind some printer driver may only be available for certain OS. Once the printer is installed, the printer can be identified under specific names and

The printing server could be packaged as either a .exe file or run as is a Flask server. The .exe format is provided in case running the server as a Python application is unfavorable. **It is recommended to create a new virutal environement before installing/ running the server.**
A requirement.txt file is included in the repository to use:
```bash
pip install requirements.txt
```
To run the printing server, go to `./PrintingServer/`:
```bash
python PrinterServer.py
```
If successful, a terminal showing the IP address of the printing server will be shown.

To convert the server to a .exe, cx_freeze can be used using the following command:
```

```
A `./PrinterServer/build` folder will be generated with a folder inside it, named depending on the host OS and machine: `./PrinterServer/build/exe.[HOST_OS]-[ARCHITECTURE]-[PYTHON_VER]`. Inside this folder the executable will be available.

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


The agent could be built using the following command:
```bash
docker-compose up -d
```



    
## Features

