package uk.ac.cam.cares.jps.agent.ceavisualisation;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import javax.servlet.annotation.WebServlet;
import java.util.Map;

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
    public static final String IRI = "iri";

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

        for (CEAVisualisationConstants column : CEAVisualisationConstants.values()) {
            createTable += column.getValue() + " DOUBLE PRECISION,";
        }

        createTable = createTable.substring(0, createTable.length()-1) + ")";

        rdbStoreClient.executeUpdate(createTable);
    }


    /**
     * Update table with CEA values used for visualisation
     * @param ceaValues map storing CEA values
     * @param iri building iri associated with ceaValues
     */
    public void updateTable(Map<String, Double> ceaValues, String iri) {
        String insert = "INSERT INTO " + SCHEMA + "." + TABLE + " (" + IRI + ",";
        String values = "VALUES (" + iri + ",";
        String conflict = "ON CONFLICT (" + IRI + ")";
        String update = "DO UPDATE SET";
        String set = "";

        for (Map.Entry<String, Double> entry : ceaValues.entrySet()) {
            insert += entry.getKey() + ",";
            values += entry.getValue() + ",";
            set += entry.getKey() + "=" + entry.getValue() + ",";
        }

        insert = insert.substring(0, insert.length()-1) + ")";
        values = values.substring(0, values.length()-1) + ")";
        set = set.substring(0, set.length()-1) + ")";

        String sql = insert + "\n" + values + "\n" + conflict + "\n" + update + "\n" + set;

        rdbStoreClient.executeUpdate(sql);
    }
}
