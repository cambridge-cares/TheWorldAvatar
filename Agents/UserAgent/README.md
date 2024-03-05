# UserAgent
## 1. Description

## 2. Pre-requisites



## 3. Agent Configuration 


## 4. Deploy 
### 4.1 Retrieving TrajectoryQueryAgent's image
The TrajectoryQueryAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/trajectoryqueryagent) using `docker pull ghcr.io/cambridge-cares/trajectoryqueryagent:<LATEST-VERSION>`

### 4.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the TrajectoryQueryAgent Docker container to be deployed in the stack. To do so, place [trajectoryqueryagent.json](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 4.3 Configuring AccessAgent endpoints 
On this same directory run, replace `STACK-NAME` with your stack-manager name.
```
./copy.sh start <STACK-NAME>
```

### 4.4 Start recording
Once all the configurations and server has been set, press the Start Recording button inside the SensorLogger mobile app to begin session. 

## 5. Build and debug
## 5.1 Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 5.2 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the TrajectoryQueryAgent local Docker Image. 

### 5.2 Spinning up with stack-manager
To debug the agent, replace [`trajectoryqueryagent-debug.json`](stack-manager-config/inputs/config/services/user-agent-debug.json) instead of [`trajectoryqueryagent.json`](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services