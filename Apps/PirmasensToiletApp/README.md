# Pirmasens Toilet App

## Ontology
Entities are defined in Miro board
![image](https://github.com/cambridge-cares/TheWorldAvatar/assets/115569120/83aa5fc0-6a73-44c5-96cd-60855469aa23)


## Screenshots

![image](https://github.com/cambridge-cares/TheWorldAvatar/assets/115569120/575660d0-183b-4e1a-8ec5-db6ed235efbf)


## Mapbox token

Your mapbox token should be in [`settings.gradle`](./gradle.properties) and also [`developer-config.xml`](./core/utils/src/main/res/values/developer-config.xml). 

## Phone and Stack communication
If you are running on emulator use `HOST_LOCALHOST` in [`NetworkConfiguration.java`](./core/network/src/main/java/uk/ac/cam/cares/jps/network/NetworkConfiguration.java), otherwise you should have android and stack on same network. You can use `ipconfig` or other related commands to find the ip address of your pc.



