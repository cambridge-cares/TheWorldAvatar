# Ample Client
This client is designed to read tags from csv files, connect to an OPC-UA server, read the values of the tags and insert them as timeseries into a PostgreSQL database.

# Set up and deployment
## Set up
1) Create csv files containing the tags to be read and insert them into the     `filtered_tags` folder. Examples of the csv files can be found in the `filtered_tags` folder.

2) In `docker-compose.yml`, under the volumes section:
-  Replace `<path on host>` with the actual location of the `config` and `filtered_tags` folders, this will mount the folders onto the container and any changes to the files in these folders will be reflected in the container automatically

3) In `./Ample_Client/config/postgres_conf.properties`, modify the parameters accordingly:
- `dbname` name of the database
- `user` username
- `password` password
- `host` IP Address of the machine hosting the database
- `port` port in which postgreSQL is listening on

4) In `./Ample_Client/config/opcua_conf.properties`, modify the parameters accordingly:
- `opcua_server_url` endpoint url of the OPC-UA server
- `user` username
- `password` password
- `interval` interval for data collection

## Deployment
Open a terminal in the same directory as this README and run the following to spin up the container:
```
docker compose up -d
```
The client will start running automatically at the interval defined in `./Ample_Client/config/opcua_conf.properties`. To change the interval, modify the interval parameter in `./Ample_Client/config/opcua_conf.properties`.