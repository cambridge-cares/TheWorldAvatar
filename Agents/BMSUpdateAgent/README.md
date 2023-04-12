# BMSUpdateAgent
[comment]: # (TODO: update the description if functionalities increased)
BMSUpdateAgent is an agent designed to change the setpoints of lab equipment, so they can be switched on or off. 
This agent corporates with ESPHomeAgent and ESPHomeUpdateAgent.
It first takes value from user and writes to the knowledge graph, which is listened by ESPHomeAgent. After changing the setpoint in the knowledge graph, this agent triggers ESPHomeAgent to check for the condition changes and toggle the state of devices accordingly. 
When the device state is set, this agent will activate ESPHomeUpdateAgent to pull data update from API to database.

At the current stage, the BMSUpdateAgent only supports changing the temperature setpoint of a cooling fan. 

# 1. Setup
This agent is designed to run in stack, which is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).
A successful setup will result in 9 containers:
- 8 [default containers](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack)
- BMSUpdateAgent

Other resources that BMSUpdateAgent depends on are running outside the stack, including ESPHomeAgent, ESPHomeUpdateAgent and the blazegraph that ESPHomeAgent read from.  
These resources need to be set up separately and their hosts should be set in the BMSUpdateAgent's config file as stated below. 

## 1.1 Config BMSUpdateAgent in Stack
### 1) Edit Agent Config Files
Replace the hosts for ESPHomeAgent, ESPHomeUpdateAgent and the blazegraph that ESPHomeAgent read from in `config/client.properties`

Replace the `<WORK_DIR>` in the `Mounts` section of `stack-manager-input-config-service/bms-update-agent.json`

### 2) Build Image
[comment]: # (TODO: the image is supposed to be pushed to the registry)
The BMSUpdateAgent is set up to use the Maven repository. You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Then build image with:
```
docker build . -t bms-update-agent:1.0.0-SNAPSHOT
```

### 3) Add Agent Config to Stack Manager
Copy `stack-manager-input-config-service/bms-update-agent.json` to `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/services/`.

Create `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it doesn't exist. If it exists already, append the agent to the file as follows:
```json
{
  "services": {
    "includes": [
      "bms-update-agent",
      ...
  ],
    "excludes": [
      ...
  ]
  }
}
```

After this step, the `stack-manager/inputs/config` folder will have the following structure:
```
config/
|_ services/
   |_ bms-update-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

## 1.2 Spin Up Stack
Follow these [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack.

# 2. Usage
The agent accepts a POST request path `/set`. The following command will set the setpoint to `<TEMPERATURE>` and turn on/off a cooling fan based on the measured temperature and the setpoint.
```
curl -X POST 'http://localhost:3838/bms-update-agent/set' \
--header 'Content-Type: application/json' \
--data '{
    "dataIRI":"https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature",
    "temperature":<TEMPERATURE>,
    "clientProperties":"CLIENT_PROPERTIES"
}'
```
If the component is in the ON state.
```json
{"fanStatus":"The fan is in the ON state.","message":"The temperature has been set to <TEMPERATURE>"}
```

If the component is in the OFF state.
```json
{"fanStatus":"The fan is in the OFF state.","message":"The temperature has been set to <TEMPERATURE>"}
```