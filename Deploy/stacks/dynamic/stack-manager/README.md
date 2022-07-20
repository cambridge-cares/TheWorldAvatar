## Spinning up the Stack

To spin up the stack (with default settings) please follow the instructions below:

0. In the Docker Desktop General Settings enable the `Expose daemon on tcp://localhost:2375 without TLS` option.
1. Open this folder in VSCode.
2. Create two files called `postgis_password` and `geoserver_password` in the `stack-manager/inputs/secrets/` directory. Populate the files with the intended passwords for postgis and geoserver, respectively.
3. Create two files called `repo_username.txt` and `repo_password.txt` in the `stack-manager/docker/credentials` directory. Populate the files with your github username and access token (i.e. with scope to write packages), respectively.
4. Initialise docker swarm by running
    ```
    docker swarm init
    ``` 
5. In the `Run and Debug` side panel of VSCode run the `Debug Stack Manager` configuration. Remarks:
   * In case not all containers start up successfully, try running the `Debug Stack Manager` configuration again
   * In case the `geoserver` container does not start up successfully (likely due to time out issues), try pulling the respective image manually by running 
    ```
   docker pull docker.cmclinnovations.com/geoserver:2.20.4
   ```
6. The Adminer and Ontop GUI endpoints should be available at `http://localhost:8080/adminer/ui/` and `http://localhost:8080/ontop/sparql/` respectivly. To check the exposed ports run
    ```
    docker servise ls
    ```

## Further remarks

* A total RAM size of 32GB is recommended for smooth execution.

* Pulling some images requires access to the Docker registry at CMCL. In case you have not gotten your credentials for that, please email `support<at>cmclinnovations.com` with the subject `Docker registry access`. Further information can be found at the [CMCL Docker Registry] wiki page. To test your access, simply run 
```
docker login docker.cmclinnovations.com
```

* To (permanently) remove all Docker containers run
    ```
    docker swarm leave --force
    ```

* To remove an individual service (e.g. geoserver), run
    ```
    docker service rm TEST-STACK-geoserver
    ```

<!-- Links -->
[CMCL Docker Registry]: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry