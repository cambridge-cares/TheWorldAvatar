# Pirmasens Toilet App

## Ontology
Entities are defined in Miro board



## Screenshots




## Mapbox token

Your mapbox token should be in [`settings.gradle`](./gradle.properties) and also [`developer-config.xml`](./core/utils/src/main/res/values/developer-config.xml). 

## Phone and Stack communication
If you are running on emulator use `HOST_LOCALHOST` in [`NetworkConfiguration.java`](./core/network/src/main/java/uk/ac/cam/cares/jps/network/NetworkConfiguration.java), otherwise you should have android and stack on same network. You can use `ipconfig` or other related commands to find the ip address of your pc.


## Uploading the Data
Place the relevant files from stack-data-uploader-inputs and stack-manager-inputs to stack-data-uploader and stack-manager folders respectively.


For spinning up a stack and uploading the data follow the instructions in [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) and [Stack Data Uploader]( https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader).


## Web Visualization
If you run the backend of the android application, you can also see the following web visualization at http://localhost:3838/visualisation/:



