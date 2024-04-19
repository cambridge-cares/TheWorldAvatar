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
        return "{" +
                "\"name\": \"" + this.sourceName + "\"," +
                "\"type\": \"postgres\"," +
                "\"url\": \"" + this.sourceUrl + "\"," +
                "\"user\": \"" + this.username + "\"," +
                "\"database\": \"" + this.databaseName + "\"," +
                "\"basicAuth\": false," +
                "\"access\": \"proxy\"," +
                "\"withCredentials\": false," +
                "\"isDefault\": false," +
                "\"jsonData\": " + this.genJsonDataConfig() +
                "\"secureJsonData\": { \"password\": \"" + this.password + "\"}" +
                "}";
    }

    /**
     * Generate the PostgreSQL specific configuration as part of a JSON data key.
     *
     * @return The JSON Data parameter for the PostgreSQL data source configuration as a String.
     */
    private String genJsonDataConfig() {
        return "{" +
                "\"tlsAuth\": false," +
                "\"connMaxLifetime\": 14400," +
                "\"maxIdleConns\": 100," +
                "\"maxIdleConnsAuto\": true," +
                "\"maxOpenConns\": 100," +
                "\"sslmode\": \"disable\"," +
                // Ensure that last line does not have comma
                "\"postgresVersion\": 1500" +
                // Note that a comma is appended because the JSON param still has another parameter after
                "},";
    }
}
