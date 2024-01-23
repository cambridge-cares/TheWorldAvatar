package com.cmclinnovations.ship;

import java.sql.Connection;
import java.util.List;

import org.jooq.CreateTableColumnStep;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.InsertValuesStep3;
import org.jooq.Record;
import org.jooq.SQLDialect;
import org.jooq.Table;
import org.jooq.impl.DSL;

public class ShipRDBClient {
    private static final SQLDialect DIALECT = SQLDialect.POSTGRES;

    private static final Table<Record> SHIP_IRI_TABLE = DSL.table(DSL.name(EnvConfig.SHIP_IRI_LOOKUP_TABLE));
    private static final Field<String> SHIP_IRI_COLUMN = DSL.field(DSL.name("ship"), String.class);
    private static final Field<String> LOCATION_IRI_COLUMN = DSL.field(DSL.name("location"), String.class);
    private static final Field<String> SHIP_NAME_COLUMN = DSL.field(DSL.name("shipname"), String.class);

    private DSLContext getContext(Connection conn) {
        return DSL.using(conn, DIALECT);
    }

    void createIriLookUpTable(Connection conn) {
        try (CreateTableColumnStep createStep = getContext(conn)
                .createTableIfNotExists(SHIP_IRI_TABLE)) {
            createStep.column(SHIP_IRI_COLUMN).column(LOCATION_IRI_COLUMN).column(SHIP_NAME_COLUMN).execute();
        }
    }

    void populateTable(List<Ship> ships, Connection conn) {
        InsertValuesStep3<Record, String, String, String> insertValueStep = getContext(conn).insertInto(SHIP_IRI_TABLE,
                SHIP_IRI_COLUMN, LOCATION_IRI_COLUMN, SHIP_NAME_COLUMN);
        ships.stream().forEach(
                ship -> insertValueStep.values(ship.getIri(), ship.getLocationMeasureIri(), ship.getShipName()));
        insertValueStep.execute();
    }
}
