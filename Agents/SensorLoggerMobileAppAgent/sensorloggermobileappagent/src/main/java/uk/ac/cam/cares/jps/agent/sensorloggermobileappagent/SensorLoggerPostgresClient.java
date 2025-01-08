package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.springframework.core.io.ClassPathResource;

import uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.processor.SensorDataProcessor;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import java.util.Set;

/**
 * column names need to match with obda file in resources
 */
public class SensorLoggerPostgresClient {
    static final String DEVICES_TABLE = "devices";
    static final String TS_QUANTITIES_TABLE = "time_series_quantities";
    static final String DEVICE_COLUMN = "device_id";
    static final String SENSOR_COLUMN = "sensor_class";

    private String dburl;
    private String dbuser;
    private String dbpassword;
    private static final Table<?> DSL_TABLE = DSL.table(DSL.name(DEVICES_TABLE));
    private static final Table<?> DSL_TABLE_TS_QUANTITIES = DSL.table(DSL.name(TS_QUANTITIES_TABLE));
    private static final Field<String> DEVICE_COLUMN_FIELD = DSL.field(DEVICE_COLUMN, String.class);
    private static final Field<String> SENSOR_COLUMN_FIELD = DSL.field(SENSOR_COLUMN, String.class);

    private static final Field<String> TS_IRI_FIELD = DSL.field("time_series_iri", String.class);
    private static final Field<String> DATA_IRI_FIELD = DSL.field("data_iri", String.class);
    private static final Field<String> COLUMN_NAME_FIELD = DSL.field("column_name", String.class);
    private static final Field<String> TABLE_NAME_FIELD = DSL.field("table_name", String.class);

    private static final Logger LOGGER = LogManager.getLogger(SensorLoggerPostgresClient.class);

    SensorLoggerPostgresClient(String dburl, String dbuser, String dbpassword) {
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
        return DriverManager.getConnection(dburl, dbuser, dbpassword);
    }

    DSLContext getContext(Connection conn) {
        return DSL.using(conn, SQLDialect.POSTGRES);
    }

    boolean populateTable(String deviceId, List<String> sensorClass) {
        boolean firstTime = false;
        try (Connection conn = getConnection(); Statement statement = conn.createStatement()) {
            String condition = String.format("table_name = '%s'", DEVICES_TABLE);

            if (getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
                    int.class) != 1) {
                // table does not exist
                firstTime = true;
                initFunctionsForGeoServer(conn);
                getContext(conn).createTable(DSL_TABLE).column(DEVICE_COLUMN_FIELD)
                        .column(SENSOR_COLUMN_FIELD).constraints(DSL.unique(DEVICE_COLUMN_FIELD, SENSOR_COLUMN_FIELD))
                        .execute();
            }

            sensorClass.forEach(s -> getContext(conn).insertInto(DSL_TABLE,
                    DEVICE_COLUMN_FIELD, SENSOR_COLUMN_FIELD).values(deviceId, s).onConflictDoNothing().execute());
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
            LOGGER.error("Error populating trajectory lookup table");
        }

        return firstTime;
    }

    /**
     * Must align the prefixes here with ontop.obda in resources!
     * These functions are used to generate the GeoServer layers
     */
    private void initFunctionsForGeoServer(Connection conn) {
        String sql = null;
        try (InputStream is = new ClassPathResource("functions.sql").getInputStream()) {
            sql = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            LOGGER.error("Failed to read functions.sql");
            LOGGER.error(e.getMessage());
        }

        if (sql != null) {
            try (Statement statement = conn.createStatement()) {
                statement.executeQuery(sql);
            } catch (SQLException e) {
                LOGGER.error("Error creating SQL functions");
                LOGGER.error(e.getMessage());
            }
        }
    }

    void linkDataIriWithTsIriInRdb(String tsIRI, Set<String> dataIRIs, SensorDataProcessor processor) {
        try (Connection conn = getConnection(); Statement statement = conn.createStatement()) {
            String tableName = getContext(conn).select(TABLE_NAME_FIELD)
                    .from(DSL_TABLE_TS_QUANTITIES)
                    .where(TS_IRI_FIELD.eq(tsIRI))
                    .fetchOne(0, String.class);

            if (tableName == null || tableName.isEmpty()) {
                LOGGER.warn("No table name found for tsIRI: " + tsIRI);
                return;
            }

            var insert = getContext(conn).insertInto(DSL_TABLE_TS_QUANTITIES,
                    TS_IRI_FIELD, DATA_IRI_FIELD, COLUMN_NAME_FIELD, TABLE_NAME_FIELD);
            for (String currentDataIRI : dataIRIs) {
                insert.values(tsIRI,
                        currentDataIRI,
                        String.format("column%d", processor.getDataIRIs().indexOf(currentDataIRI)),
                        tableName);
            }
            insert.onConflictDoNothing().execute();
        } catch (SQLException e) {
            LOGGER.error("Error linking dataIRI with tsIRI");
            LOGGER.error(e.getMessage());
        }
    }
}
