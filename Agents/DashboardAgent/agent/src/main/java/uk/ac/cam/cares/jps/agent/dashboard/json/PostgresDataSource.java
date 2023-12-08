package uk.ac.cam.cares.jps.agent.dashboard.json;

/**
 * A Java representation of a JSON-like model that encapsulates and enforces information about syntax
 * specific to Grafana's data source API for PostgreSQL databases.
 *
 * @author qhouyee
 */
public class PostgresDataSource {
    private final String sourceName;
    private final String sourceUrl;
    private final String username;
    private final String password;
    private final String databaseName;

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
        this.sourceName = name;
        this.sourceUrl = url;
        this.username = user;
        this.password = password;
        this.databaseName = databaseName;
    }

    /**
     * Construct the JSON parameters as a String.
     *
     * @return The JSON parameter for a PostgreSQL data source as a String.
     */
    protected String construct() {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                .append("\"name\": \"").append(this.sourceName).append("\",")
                .append("\"type\": \"postgres\",")
                .append("\"url\": \"").append(this.sourceUrl).append("\",")
                .append("\"user\": \"").append(this.username).append("\",")
                .append("\"database\": \"").append(this.databaseName).append("\",")
                .append("\"basicAuth\": false,")
                .append("\"access\": \"proxy\",")
                .append("\"withCredentials\": false,")
                .append("\"isDefault\": false,")
                .append("\"jsonData\": ").append(this.genJsonDataConfig())
                .append("\"secureJsonData\": { \"password\": \"").append(this.password).append("\"}")
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
