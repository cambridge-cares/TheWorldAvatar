
This deploys the NTU campus visualisation, which runs separately to the NTU stack. For full functionality the NTU stack must be deployed and the Grafana dashboard needs to be set up and embedded into the visualisation.

# Requirements: 
- Create Mapbox credentials and add the files 'mapbox_api_key' and 'mapbox_username' to the webspace directory 
- Place the following .b3dm files in webspace/tiles:
    - 0.b3dm
    - 1.b3dm

# Deploying the visualisation
In this folder, run the command
`docker-compose up -d`