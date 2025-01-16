package uk.ac.cam.cares.jps.agent.buildingflooragent;

import java.sql.Connection;

import org.jooq.CreateSchemaFinalStep;
import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.Record;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;

public class FloorPostGISClient {
    private static final SQLDialect DIALECT = SQLDialect.POSTGRES;
    private static final Field<String> FLOOR_CAT_COLUMN = DSL.field(DSL.name("floor_category"), String.class);
    private static final Field<Integer> NUM_FLOORS_COLUMN = DSL.field(DSL.name("num_floors"), Integer.class);
    private static final Field<String> BUILDING_UUID_COLUMN = DSL.field(DSL.name("building_uuid"), String.class);
    private static final String TABLE_NAME = "floors";
    private static final String SCHEMA_NAME = "gfa_floors";
    private static final Table<Record> FLOORS_TABLE = DSL.table(DSL.name(SCHEMA_NAME, TABLE_NAME));

    private FloorPostGISClient() {
        throw new IllegalStateException("FloorPostGISClient");
    }

    static void createSchema(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        try (CreateSchemaFinalStep step = context.createSchemaIfNotExists(SCHEMA_NAME)) {
            step.execute();
        }
    }

    static void createTable(Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        try (CreateTableColumnStep step = context.createTableIfNotExists(FLOORS_TABLE)) {
            step.column(FLOOR_CAT_COLUMN).column(NUM_FLOORS_COLUMN).column(BUILDING_UUID_COLUMN).execute();
        }

        context.createUniqueIndex().on(FLOORS_TABLE, BUILDING_UUID_COLUMN).execute();
    }

    static void insertData(String buildingUuid, String floorCat, int numFloors, Connection conn) {
        DSLContext context = DSL.using(conn, DIALECT);

        // if a row with the building uuid exists, it will ignore that row
        context.insertInto(FLOORS_TABLE, BUILDING_UUID_COLUMN, FLOOR_CAT_COLUMN, NUM_FLOORS_COLUMN)
                .values(buildingUuid, floorCat, numFloors).onConflictDoNothing().execute();
    }
}
