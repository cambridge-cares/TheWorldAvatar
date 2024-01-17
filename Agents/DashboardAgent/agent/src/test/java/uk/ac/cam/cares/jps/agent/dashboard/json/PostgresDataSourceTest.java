package uk.ac.cam.cares.jps.agent.dashboard.json;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PostgresDataSourceTest {
    private static final String SAMPLE_SOURCE_NAME = "postgres_123";
    private static final String SAMPLE_SOURCE_URL = "host:3810";
    private static final String SAMPLE_USERNAME = "postgres";
    private static final String SAMPLE_PASSWORD = "postgis";
    private static final String SAMPLE_DATABASE_NAME = "test";

    @Test
    void testConstruct() {
        PostgresDataSource dataSource = new PostgresDataSource(SAMPLE_SOURCE_NAME, SAMPLE_SOURCE_URL, SAMPLE_USERNAME, SAMPLE_PASSWORD, SAMPLE_DATABASE_NAME);
        String results = dataSource.construct();
        assertEquals(genExpectedResults(SAMPLE_SOURCE_NAME, SAMPLE_SOURCE_URL, SAMPLE_USERNAME, SAMPLE_PASSWORD, SAMPLE_DATABASE_NAME), results);
    }

    private static String genExpectedResults(String name, String url, String user, String password, String databaseName) {
        StringBuilder builder = new StringBuilder();
        builder.append("{")
                .append("\"name\": \"").append(name).append("\",")
                .append("\"type\": \"postgres\",")
                .append("\"url\": \"").append(url).append("\",")
                .append("\"user\": \"").append(user).append("\",")
                .append("\"database\": \"").append(databaseName).append("\",")
                .append("\"basicAuth\": false,")
                .append("\"access\": \"proxy\",")
                .append("\"withCredentials\": false,")
                .append("\"isDefault\": false,")
                .append("\"jsonData\": ")
                .append("{\"tlsAuth\": false,\"connMaxLifetime\": 14400,\"maxIdleConns\": 100,\"maxIdleConnsAuto\": true,")
                .append("\"maxOpenConns\": 100,\"sslmode\": \"disable\",\"postgresVersion\": 1500},")
                .append("\"secureJsonData\": { \"password\": \"").append(password).append("\"}")
                .append("}");
        return builder.toString();
    }
}