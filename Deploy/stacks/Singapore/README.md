# Augmented Singapore
This repository contains the instructions, directory structure, and configurations required to deploy the Singapore stack. 

## 1. Preparations
### Knowledge of the stack tools adopted in The World Avatar
Please read through the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) and [Stack Data Uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader) to set up your stack accordingly.

At the moment, a working understanding of these two tools will suffice for the deployment of the Singapore stack.

## 2. Deployment Workflow
### Stack manager
1) Please copy the `data` and `config` directories from the `./stack-manager/inputs/` into the respective directories on your deployment directory
2) In the `config` directory, please rename the `sg.json` to your preferred stack name ie `STACK-NAME.JSON`, if necessary.
3) In the `data` directory, 
    - Add the mapbox credentials to the `data/webspace/3d` directory to ensure the visualisation for the 3d route will function. 
    - At the moment, the buildings ABox have not been developed but their query is available in the `data/fia` directory.
    - Please replace all the `stackendpoint` phrase into the stack's endpoint within the `data.json` in both `data/webspace` and `data/webspace/3d`, as well as the `index.html` in the `data/webspace`. Simply replace and do not add an additional `/` or things will break.
4) Please start the stack:
```
./stack.sh start <STACK NAME>
```

### Stack data uploader
1) Please copy the `data` and `config` directories from the `./stack-data-uploader/inputs/` into the respective directories on your deployment directory
2) In the `config` directory, please rename the `sg.json` to your preferred stack name ie `STACK-NAME.JSON`, if necessary. If you are instantiating the data for the first time, please ensure that the `skip` modifiers are all set to `false` or they will be ignored.
3) Please populate the `data` directory with the respective data on the Dropbox folder
4) Please start your stack if you haven't done so, and then run the following lines in your `stack-data-uploader` deployment directory:
```
./stack.sh start <STACK NAME>
```

## 3. Miscellaneous Functions
### Agents
Please ensure that the visualisation, feature-info-agent, and filter-agent services are deployed in the stack. The Plot Finder requires these three services to function.

### Legend
The mapbox visualisation can currently generate legends for different parameters manually. Please  check out the `manager.getPanelHandler().setLegend(htmlContent);` line at the `./stack-manager/inputs/data/webspace/index.html`.
New sets of gradient bars can be generated in the `./stack-manager/inputs/data/webspace/component/legend.css`.
