# GFAAgent
## 1. Description
This agent has been developed to compute the Gross Floor Area (GFA) and construction cost of buildings. Presently, the agent performs the following two tasks:
1) Calculate the Gross Floor Area of buildings through multiplying the area of the building footprint with the number of storeys
2) Calculate the construction cost of buildings by GFA and standard unit price of construction cost.

### 1.1 Requirements
1) The agent requires 3D building models based on the CityGML standard. These models must be uploaded through the [stack-data-uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#citydb-data).
2) The agent requires number of floors data. Therefore, the Building Floor Agent should be run firstly.
3) Construction cost standard unit price: The latest standard construction cost should be integrated in csv file named as [cost.csv], which need to upload to postgresql by stack-data-uploader first.

## 2. GFA Agent
The agent is designed for execution through a Docker container. Other deployment workflows are beyond the scope of this document. Follow the steps below to build and deploy the agent.
### 2.1 Preparation
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

### 2.2 Retrieving GFAAgent's image
The GFAagent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/gfaagent) using `docker pull ghcr.io/cambridge-cares/gfaagent:<LATEST-VERSION>`

### 2.3 Starting with the stack-manager
The agent has been implemented to work in the stack. To do so, place gfaagent.json in the [stack-manager config directory]. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 2.4 Running the Agent
The agent is reachable at four endpoints:
1) `/calculation`: calculate GFA
2) `/calculationwithodba`: calculate GFA and upload gfa.odba
3) `/cost`: calculate construction cost
4) `/costwithodba`: calculate construction cost and upload cost.odba


To run the agent, run the following cURL command:
```
curl -X POST localhost:3838/gfaagent/calculation
curl -X POST localhost:3838/gfaagent/cost
```

