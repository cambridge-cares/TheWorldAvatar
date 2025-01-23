# UserAgent
## 1. Description
UserAgent is an agent that manages TWA user with other services account or devices (e.g. smartphones). It currently supports the following functions through the endpoints:
- `/registerPhone`: register a new phone with the user
- `/getPhoneIds`: get the phone ids of the user
- `/status`: get the status of the agent. Used for testing.

## 2. Requirements
Launch stack with the default containers and the following additional containers:
- keycloak

## 3. Deploy 
### 3.1 Retrieving UserAgent's image
The UserAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/user-agent) using `docker pull ghcr.io/cambridge-cares/user-agent:<LATEST-VERSION>`

### 3.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the UserAgent Docker container to be deployed in the stack. To do so, place [user-agent.json](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) main folder. This will spin up the agent in the stack.

## 4. Build and debug
## 4.1 Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 4.2 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the TrajectoryQueryAgent local Docker Image. 

### 4.2 Spinning up with stack-manager
To debug the agent, replace [`user-agent-debug.json`](stack-manager-config/inputs/config/services/user-agent-debug.json) instead of [`user-agent.json`](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services