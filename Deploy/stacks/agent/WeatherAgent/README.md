To deploy the WeatherAgent in the agent stack:

1. Copy the contents of secrets.template to secrets
2. Populate secrets/credentials.properties.dev and/or secrets/credentials.properties.prod .
3. Deploy the WeatherAgent on its own:
```
  start-stack.sh agent [mode] weather-agent
```
or the whole stack:
```
  start-stack.sh agent [mode]
```