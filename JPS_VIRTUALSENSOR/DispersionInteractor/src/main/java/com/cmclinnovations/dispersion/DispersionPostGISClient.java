package com.cmclinnovations.dispersion;

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
import org.postgis.Point;
import org.postgis.Polygon;

public class DispersionPostGISClient {
    private String dburl;
    private String dbuser;
    private String dbpassword;
    private Table<?> table = DSL.table(DSL.name(Config.SCOPE_TABLE_NAME));
    private static final Logger LOGGER = LogManager.getLogger(DispersionPostGISClient.class);

    DispersionPostGISClient(String dburl, String dbuser, String dbpassword) {
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

    boolean tableExists(String tableName, Connection conn) {
        String condition = String.format("table_name = '%s'", tableName);
        return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
                int.class) == 1;
    }

    void createTable(String tableName, Connection conn) throws SQLException {
        String sqlTemplate = "CREATE TABLE %s (" +
                "iri character varying," +
                "geom geometry," +
                "main_uuid character varying," +
                "geom_uuid character varying)";

        String sql = String.format(sqlTemplate, tableName);
        try (Statement stmt = conn.createStatement()) {
            stmt.executeUpdate(sql);
        }
    }

    Polygon getPolygonAs4326(Polygon polygon, Connection conn) {
        String sqlTemplate = String.format("SELECT ST_AsEWKT(ST_Transform(ST_GeomFromText('%s'), 4326))",
                polygon.toString());

        try (Statement stmt = conn.createStatement()) {
            ResultSet result = stmt.executeQuery(sqlTemplate);
            while (result.next()) {
                return new Polygon(result.getString("st_asewkt"));
            }
            throw new RuntimeException("Empty result from convertPolygonTo4326");
        } catch (SQLException e) {
            LOGGER.error("SQL state: {}", e.getSQLState());
            LOGGER.error(e.getMessage());
            throw new RuntimeException("Failed to convert polygon to EPSG:4326", e);
        }
    }

    boolean scopeExists(Polygon polygon, Connection conn) {
        boolean scopeExists = false;
        if (polygon != null) {
            String sql = String.format("SELECT ST_Equals(geom, ST_GeomFromText('%s')) FROM %s", polygon.toString(),
                    Config.SCOPE_TABLE_NAME);
            try (Statement stmt = conn.createStatement()) {
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    if (result.getBoolean("st_equals")) {
                        scopeExists = true;
                        break;
                    }
                }
            } catch (SQLException e) {
                LOGGER.error("SQL state: {}", e.getSQLState());
                LOGGER.error(e.getMessage());
                scopeExists = true; // set to true so that the code won't proceed
            }
        }
        return scopeExists;
    }

    String getScopeIri(Polygon polygon, Connection conn) {
        String scopeIri = null;
        String sql = String.format("SELECT iri FROM %s WHERE ST_Equals(geom, ST_GeomFromText('%s'))",
                Config.SCOPE_TABLE_NAME, polygon.toString());

        try (Statement stmt = conn.createStatement()) {
            ResultSet result = stmt.executeQuery(sql);
            // there should not be any duplicates
            while (result.next()) {
                scopeIri = result.getString("iri");
            }
        } catch (SQLException e) {
            LOGGER.error("SQL state: {}", e.getSQLState());
            LOGGER.error(e.getMessage());
        }

        return scopeIri;
    }

    boolean sensorExists(Point target, Connection conn) {
        if (!tableExists(Config.SENSORS_TABLE_NAME, conn))
            return false;

        // Critical distance is arbitrarily set to 0.05.
        String sql = String.format(
                "SELECT scope_iri from %S WHERE ST_DISTANCE(wkb_geometry, ST_GeomFromText('%s')) <= %f ",
                Config.SENSORS_TABLE_NAME, target.toString(), 0.05);
        try (Statement stmt = conn.createStatement()) {
            ResultSet result = stmt.executeQuery(sql);
            if (result.first())
                return true;
        } catch (SQLException e) {
            LOGGER.error("SQL state: {}", e.getSQLState());
            LOGGER.error(e.getMessage());
        }

        return false;
    }

    String addScope(Polygon polygon, Connection conn) {
        String scopeIri = null;
        try {
            String scopeUuid = UUID.randomUUID().toString();
            scopeIri = QueryClient.PREFIX + scopeUuid;
            String geomUuid = UUID.randomUUID().toString();
            InsertValuesStepN<?> insertStep = getContext(conn).insertInto(table).values(scopeIri, polygon, scopeUuid,
                    geomUuid);
            try (Statement stmt = conn.createStatement()) {
                stmt.executeUpdate(insertStep.toString());
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Failed to parse EWKT literal");
        }
        return scopeIri;
    }
}
