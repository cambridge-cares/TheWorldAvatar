package uk.ac.cam.cares.jps.agent.ceavisualisation;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import javax.servlet.annotation.WebServlet;

@WebServlet(
        urlPatterns = {
                CEAVisualisationAgent.URI_RUN})
public class CEAVisualisationAgent extends JPSAgent {
    public static final String URI_RUN = "/run";

    public final String DB_NAME;
    public final String DB_USER;
    public final String DB_PASSWORD;
    public RemoteRDBStoreClient rdbStoreClient;
    public static final String SCHEMA = "ceavis";
    public static final String TABLE = "cea";

    public CEAVisualisationAgent() {
        EndpointConfig endpointConfig = new EndpointConfig();
        DB_USER = endpointConfig.getDbUser();
        DB_PASSWORD = endpointConfig.getDbPassword();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(DB_NAME), DB_USER, DB_PASSWORD);
        initialiseTable();
    }

    /***
     * Intialise table to be used for visualisation in TWA-VF
     */
    public void initialiseTable() {
        // create schema
        String createSchema = "CREATE SCHEMA IF NOT EXISTS " + SCHEMA;

        rdbStoreClient.executeUpdate(createSchema);

        // create table
        String createTable = "CREATE TABLE IF NO EXISTS " + SCHEMA + "." + TABLE + "("
                + "building_iri VARCHAR(4000),\n";

        for (String column : CEAVisualisationConstants.columns) {
            createTable += column + "DOUBLE PRECISION,";
        }

        createTable = createTable.substring(0, createTable.length()-1) + ")";

        rdbStoreClient.executeUpdate(createTable);
    }
}
