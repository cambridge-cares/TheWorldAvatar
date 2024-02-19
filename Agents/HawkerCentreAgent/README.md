# Hawker Centre Agent

## Deploy and Run

### Deployment
Modify the `client.properties` file to include the appropriate endpoints for storing the hawker centre information.

Open a command terminal in the same directory as this readme and run the following:
```
docker compose up -d
```

### To run the agent
```
curl -X POST --header "Content-Type: application/json" -d "{\"apiProperties\":\"APIPROPERTIES\",\"clientProperties\":\"CLIENTPROPERTIES\"}" http://localhost:1070/hawker-centre-agent/retrieve
```