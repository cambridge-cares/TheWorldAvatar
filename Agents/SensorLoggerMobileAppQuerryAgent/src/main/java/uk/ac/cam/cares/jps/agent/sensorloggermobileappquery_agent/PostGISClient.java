package uk.ac.cam.cares.jps.agent.sensorloggermobileappquery_agent;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.InsertValuesStepN;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.postgis.Polygon;

public class PostGISClient {
    private String dburl;
    private String dbuser;
    private String dbpassword;
    private Table<?> table = DSL.table(DSL.name(Config.SCOPE_TABLE_NAME));
    private static final Logger LOGGER =  LogManager.getLogger(PostGISClient.class);

    PostGISClient(String dburl, String dbuser, String dbpassword) {
        this.dburl = dburl;
        this.dbuser = dbuser;
        this.dbpassword = dbpassword;
    }

    public Connection getConnection() throws SQLException {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to load Postgres driver");
        }
        return DriverManager.getConnection(this.dburl, this.dbuser, this.dbpassword);
    }
    DSLContext getContext(Connection conn) {
        return DSL.using(conn, SQLDialect.POSTGRES);
    }
}
