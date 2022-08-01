# Vapourtec Agent
The folder contains the source, resource, and Docker setup files for the Vapourtec Agent, following the suggestions on a template provide as `TheWorldAvatar/JPS_BASE_LIB/python_derivation_agent/README.md`.

# Note for dockerised test
FlowCommander instance needs to be opened on the host machine to allow the deployed Vapourtec Agent to update the hardware state. Otherwise the assertion that checks the `stateLastUpdatedAt` of Vapourtec will fail.

# Note for deployment
**NOTE THAT DEVELOPER WHO DEPLOYS THE AGENT _MUST_ MAKE SURE THE FlowCommander INSTANCE INSTALLED ON THE HOST MACHINE IS STARTED BEFORE THE DEPLOYMENT OF THIS AGENT.**
```cmd
cd D:\Vapourtec\FCEXP
docker run -v "D:\Vapourtec\FCEXP:/app/vapourtec" --env-file agent.vapourtec.env --add-host=localhost:host-gateway --name vapourtec_agent ghcr.io/cambridge-cares/vapourtec_agent:1.0.0-SNAPSHOT
```

# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
