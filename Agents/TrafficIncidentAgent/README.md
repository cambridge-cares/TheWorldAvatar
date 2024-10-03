# Traffic Incident Agent

This agent downloads real-time traffic-incident data from [http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents](http://datamall2.mytransport.sg/ltaodataservice/TrafficIncidents), processes data to wanted format, and stores them in Postgre database in the stack.

## Building and running

This section specifies the minimum requirement to build the docker image.

This agent uses the Maven repository at [https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:

```
credentials/
  repo_password.txt
  repo_username.txt
```

`repo_username.txt` should contain your GitHub username, and `repo_password.txt` contains your GitHub [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

In the same folder, you will also need to have an `api_key.txt` which can be obtained by registering from [Land Transport Data Mall](https://datamall.lta.gov.sg/content/datamall/en/request-for-api.html). Remember to copy the API key to the `config/api.properties` to ensure the data can be correctly retrieved and remember not to publish your key on GitHub.

You also need to have a copy of your credentials in single-word text files located like below:

```
docker/
  credentials/
    repo_password.txt
    repo_username.txt
```

This agent is designed to work with a stack from CMCL. Refer to [this link](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) to find out how to set up a stack. After running the command `./stack.sh build` and then `./stack.sh start <STACK NAME>` at the folder same as the link, you should be able to see a container named as `<STACK NAME>` as you specified earlier. Now you can proceed to build the image here and start it as a service by running the same two commands, but at the current folder.

While you have the container running, you do not need to create any database or table as it is already automated. The PostGIS extension is also automatically enabled. You can view the data in stack. By opening the Adminer (PostgreSQL GUI) at http://localhost:3838/adminer/ui/?username=postgres&pgsql=. Enter `<STACK NAME>-postgis:5432` as the Server and the value from the postgis_password file as the Password. The Database slot is the default `postgres` and the table is named as `TrafficIncident`. The table should include `starttime:bigint`, `endtime:bigint`, `type:character varing`, `message:character varying`, `latitude:double precision`, `longitude:double precision`, `location: geography NULL`, `status:Boolean`.

After having the container running and setting up the table as described, you can send a `POST` query with url `http://localhost:1016/traffic-incident-agent/retrieve` to get the agent running and deposit values into Postgres. One of the recommended ways is to work with Postman and build a query from there.

The modeling of the Traffic Incidents is achieved by maintaining a time interval to track the start and end time of the incident. When the incident first appears, the end time field will be left as 0 and only gets updated when the incident is not appearing in the newly queried result. Hence, the accuracy of data needs to be maintained via having regular call of the query.
