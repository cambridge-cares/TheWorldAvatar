package uk.ac.cam.cares.jps.agent.gfaagent;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.jooq.CreateSchemaFinalStep;
import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.Record;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;
import org.springframework.core.io.ClassPathResource;
import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class GFAPostGISClient {
    private static final Logger LOGGER = LogManager.getLogger(GFAPostGISClient.class);
    private static final SQLDialect DIALECT = SQLDialect.POSTGRES;
    private static final String SCHEMA_NAME = "gfa_floors";
    private static final String GFA_TABLE_NAME = "gfa";
    private static final String COST_TABLE_NAME = "cost";
    private static final Table<Record> GFA_TABLE = DSL.table(DSL.name(SCHEMA_NAME, GFA_TABLE_NAME));
    private static final Table<Record> COST_TABLE = DSL.table(DSL.name(SCHEMA_NAME, COST_TABLE_NAME));
    private static final Field<String> BUILDING_UUID_COLUMN = DSL.field(DSL.name("building_uuid"), String.class);
    private static final Field<Double> GFA_COLUMN = DSL.field(DSL.name("gfa"), Double.class);
    private static final Field<Double> COST_COLUMN = DSL.field(DSL.name("cost"), Double.class);

    private GFAPostGISClient() {
        throw new IllegalStateException("GFAPostGISClient");
    }

    static void createSchema(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        try (CreateSchemaFinalStep step = context.createSchemaIfNotExists(SCHEMA_NAME)) {
            step.execute();
        }
    }

    static void createGFATable(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        try (CreateTableColumnStep step = context.createTableIfNotExists(GFA_TABLE)) {
            step.column(BUILDING_UUID_COLUMN).column(GFA_COLUMN).execute();
        }
        context.createUniqueIndex().on(GFA_TABLE, BUILDING_UUID_COLUMN).execute();
    }

    static void addGFAData(String buildingIri, int numberFloor, Connection conn) {
        double area = getTotalAreaOfBuilding(buildingIri, conn);
        double gfa = area * numberFloor;

        String uuid = extractUuid(buildingIri);

        DSLContext context = DSL.using(conn, DIALECT);
        context.insertInto(GFA_TABLE).columns(BUILDING_UUID_COLUMN, GFA_COLUMN).values(uuid, gfa).onConflictDoNothing()
                .execute();
    }

    static double getTotalAreaOfBuilding(String buildingUuid, Connection conn) {
        String sqlTemplate;
        try (InputStream is = new ClassPathResource("total_area_query.sql").getInputStream()) {
            sqlTemplate = IOUtils.toString(is, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        String query = String.format(sqlTemplate, buildingUuid);

        double area = 0.0;
        try (Statement stmt = conn.createStatement()) {
            ResultSet queryResult = stmt.executeQuery(query);

            while (queryResult.next()) {
                area = queryResult.getDouble("total_area");
            }
        } catch (SQLException e) {
            throw new RuntimeException("Error while getting total area of building", e);
        }

        if (area == 0.0) {
            LOGGER.warn("Area for building {} is zero", buildingUuid);
        }
        return area;
    }

    static void createCostTable(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);
        try (CreateTableColumnStep step = context.createTableIfNotExists(COST_TABLE)) {
            step.column(BUILDING_UUID_COLUMN).column(COST_COLUMN).execute();
        }
        context.createUniqueIndex().on(COST_TABLE, BUILDING_UUID_COLUMN).execute();
    }

    static void addCostData(String buildingIri, double cost, Connection conn) {
        String buildingUuid = extractUuid(buildingIri);

        DSLContext context = DSL.using(conn, DIALECT);
        context.insertInto(COST_TABLE).columns(BUILDING_UUID_COLUMN, COST_COLUMN).values(buildingUuid, cost)
                .onConflictDoNothing().execute();
    }

    static String extractUuid(String buildingIri) {
        try {
            // Create a URL object
            URL url = new URL(buildingIri);

            // Get the path of the URL
            String path = url.getPath();

            // Split the path by '/'
            String[] pathSegments = path.split("/");

            // The UUID is the last segment of the path
            return pathSegments[pathSegments.length - 1];
        } catch (Exception e) {
            throw new RuntimeException("Error extracting uuid from building IRI", e);
        }
    }
}
