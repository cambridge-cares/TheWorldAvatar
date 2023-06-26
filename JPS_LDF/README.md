The Dockerfile in this directory can be used to deploy an LDF server, which caches SPARQL query results requested by the [JPS Chatbot](../JPS_Chatbot/README.md).

The LDF server was created by Xiaochi Zhou (xz378@cam.ac.uk).

## Prerequisites

To run the LDF server, two `.hdt` files are required. The files are archived in `http://www.theworldavatar.com/MARIE_DEPLOYMENT`, named `ontocompchem_clean.hdt` and `ontokin_clean.hdt`.
The files should be downloaded and put into `$CATALINA_HOME/bin/` of the Tomcat server. The `.hdt` files are compressed RDF data, to make the `.hdt` files, 
download the `HDT-it` tool from `https://www.rdfhdt.org/downloads/`. Detailed instructions can be found on the same website. 

## Building the image and running a container
1. Create a file at ./docker/credentials/repo_username.txt and add your github username.
2. Create a file at ./docker/credentials/repo_password.txt and add your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token). The token must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).
3. Ensure localhost port 53001 is not already in use
4. On the command line, in this directory, run 'docker-compose up' (or right-click docker-compose.yml in VS Code and select 'Compose Up')

## Testing the container 
1. In a browser, visit localhost:53001. You should see "Cannot GET /"
2. Run the Python script at ../JPS_Chatbot/UI/source/JPS_Query/test/JPS_query_constructor_test.py, which submits an example query to the server. A result should be returned in 1-2 minutes.
3. Run the Python script again - it should now take less than 5 seconds to return the same result.

## Update on CACHE_SERVER (2023.06)
The `components/CACHE_SERVER` is a `node.js` implementation of a SPARQL endpoint, which converts SPARQL queries to LDF requests. In Marie 3.0, the SPARQL queries are disregarded. As a result, 
the `CACHE_SERVER` interface is removed from the workflow of Marie and the Marie system will directly interact with the `LDF_SERVER`

 