package uk.ac.cam.cares.jsp.linking;

import java.sql.SQLException;

class PostgresClientTest {

    private final String url = "jdbc:postgresql://localhost/sg_ntu";
    private final String user = "postgres";
    private final String password = "123456";
    @org.junit.jupiter.api.Test
    void getConnection() {
        // PostgresClient conn = new PostgresClient(url, user, password);
        // try {
        //     // conn.getConnection();
        //     System.out.println("Connected to the PostgreSQL server successfully.");
        // } catch (SQLException e) {
        //     System.out.println(e.getMessage());
        // }
    }

}