package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options;

/**
 * A helper to create data source json syntax.
 *
 * @author qhouyee
 */
public class DataSource {
    private final String databaseConnectionId;
    private final String sourceType;

    /**
     * Constructor to initialise a PostgreSQL data source.
     *
     * @param databaseConnectionId The uid for the database connection.
     */
    public DataSource(String databaseConnectionId) {
        this.sourceType = "postgres";
        this.databaseConnectionId = databaseConnectionId;
    }

    /**
     * Constructor to initialise the default Grafana data source.
     */
    public DataSource() {
        this.sourceType = "datasource";
        this.databaseConnectionId = "grafana";
    }

    /**
     * Construct the data source into the Grafana compliant JSON syntax.
     *
     * @return The JSON Data source syntax as a String.
     */
    public String construct() {
        return "\"datasource\": {\"type\": \"" + this.sourceType + "\", \"uid\": \"" + this.databaseConnectionId + "\"},";
    }
}
