package uk.ac.cam.cares.jps.agent.dashboard.json;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information about syntax
 * specific to Grafana's data source API for PostgreSQL databases.
 *
 * @author qhouyee
 */
public class PostgresDataSource {
    private final String SOURCE_NAME;
    private final String SOURCE_URL;
    private final String USERNAME;
    private final String PASSWORD;
    private final String DATABASE_NAME;

    /**
     * Constructor to set up required syntax for the data source HTTP API.
     *
     * @param name         The name for this data source. This is up to the user to customise.
     * @param url          The remote RDB endpoint.
     * @param user         Username to access this RDB.
     * @param password     Password to access this RDB.
     * @param databaseName The specific database to connect to.
     */
    public PostgresDataSource(String name, String url, String user, String password, String databaseName) {
        this.SOURCE_NAME = name;
        this.SOURCE_URL = url;
        this.USERNAME = user;
        this.PASSWORD = password;
        this.DATABASE_NAME = databaseName;
    }

    /**
     * Construct the JSON parameters as a String.
     *
     * @return The JSON parameter for a PostgreSQL data source as a String.
     */
    protected String construct() {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                .append("\"name\": \"").append(this.SOURCE_NAME).append("\",")
                .append("\"type\": \"postgres\",")
                .append("\"url\": \"").append(this.SOURCE_URL).append("\",")
                .append("\"user\": \"").append(this.USERNAME).append("\",")
                .append("\"database\": \"").append(this.DATABASE_NAME).append("\",")
                .append("\"basicAuth\": false,")
                .append("\"access\": \"proxy\",")
                .append("\"withCredentials\": false,")
                .append("\"isDefault\": false,")
                .append("\"jsonData\": ").append(this.genJsonDataConfig())
                .append("\"secureJsonData\": { \"password\": \"").append(this.PASSWORD).append("\"}")
                .append("}");
        return builder.toString();
    }

    /**
     * Generate the PostgreSQL specific configuration as part of a JSON data key.
     *
     * @return The JSON Data parameter for the PostgreSQL data source configuration as a String.
     */
    private String genJsonDataConfig() {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                .append("\"tlsAuth\": false,")
                .append("\"connMaxLifetime\": 14400,")
                .append("\"maxIdleConns\": 100,")
                .append("\"maxIdleConnsAuto\": true,")
                .append("\"maxOpenConns\": 100,")
                .append("\"sslmode\": \"disable\",")
                // Ensure that last line does not have comma
                .append("\"postgresVersion\": 1500")
                // Note that a comma is appended because the JSON param still has another parameter after
                .append("},");
        return builder.toString();
    }
}
