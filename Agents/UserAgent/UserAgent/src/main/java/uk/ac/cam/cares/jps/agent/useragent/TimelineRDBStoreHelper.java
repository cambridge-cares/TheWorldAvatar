package uk.ac.cam.cares.jps.agent.useragent;

import org.apache.log4j.Logger;

import org.jooq.DSLContext;
import org.jooq.Name;
import org.jooq.Query;
import org.jooq.SQLDialect;
import org.jooq.conf.ParamType;
import org.jooq.impl.SQLDataType;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import static org.jooq.impl.DSL.*;

public class TimelineRDBStoreHelper {
    private RemoteRDBStoreClient postgresRdbClient;
    private Logger LOGGER = Logger.getLogger(TimelineRDBStoreHelper.class);
    private DSLContext context;

    public TimelineRDBStoreHelper(RemoteRDBStoreClient postgresRdbClient) {
        this.postgresRdbClient = postgresRdbClient;
        context = using(SQLDialect.POSTGRES);
        initRDBStore();
    }

    private void initRDBStore() {
        initTimelineSchema();
        initPhoneTable();
    }

    private void initTimelineSchema() {
        Query query = context.select(field("schema_name"))
                .from(table(name("information_schema", "schemata")))
                .where(field("schema_name").eq(val("timeline")));
        JSONArray result = postgresRdbClient.executeQuery(query.getSQL(ParamType.INLINED));

        LOGGER.debug(result);
        if (!result.isEmpty()) {
            return;
        }

        LOGGER.debug("creating schema");
        postgresRdbClient.executeUpdate("CREATE SCHEMA timeline;");
    }

    private void initPhoneTable() {
        Query queryCreateTable = context.createTableIfNotExists(name("postgres", "timeline", "smartPhone"))
                .column(name("phone_id"), SQLDataType.CHAR.length(36))
                .column(name("user_id"), SQLDataType.CHAR.length(36))
                .constraints(constraint("pk_phone_id").primaryKey(name("phone_id")));
        postgresRdbClient.executeUpdate(queryCreateTable.getSQL());
    }

    public void registerPhone(String phoneId, String userId) {
        Query query = context.insertInto(table(name("timeline", "smartPhone")))
                        .columns(field(name("phone_id")), field(name("user_id")))
                        .values(val(phoneId), val(userId));
        postgresRdbClient.executeUpdate(query.getSQL(ParamType.INLINED));
    }

    public JSONArray getExistingPhoneIdRecord(String phoneId) {
        Query query = context.select(field("user_id"))
                .from(table(name("postgres", "timeline", "smartPhone")))
                .where(field("phone_id").eq(val(phoneId)));
        return postgresRdbClient.executeQuery(query.getSQL(ParamType.INLINED));
    }
}
