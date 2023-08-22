# OSMAgent
## Description
The OSMAgent is intended to do
1) Building matching from building footprint.
2) Categorize OSM data as according to OntoBuiltEnvironment
3) Create column and populate columns. 

## Pre-requisite
Provide OSM data in the form of .gml data. 

## To deploy this agent with the stack locally
1) Prepare the stack-manager
The agent has been implemented to work with stack, which requires the stack to be [set up](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

2) Input the necessary credentials in the folders

You'll need to provide  your credentials in single-word text files located like this:
#### Under the main folder
```
./OSMAgent/
    credentials/
        repo_username.txt
        repo_password.txt
```

