package uk.ac.cam.cares.jps.agent.trafficincident;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;
import org.jooq.DSLContext;
import org.jooq.Field;
import org.jooq.impl.DSL;
import org.jooq.InsertValuesStepN;
import org.jooq.SQLDialect;
import org.jooq.Table;

public class TrafficIncidentPostgresAgent {
    private final Logger LOGGER = LogManager.getLogger(TrafficIncidentPostgresAgent.class);

    public static final String SQL_UPDATE_ERROR_MSG = "Fail to update the record";
    public static final String SQL_INITIALIZE_ERROR_MSG = "Fail to create table in Postgres database";
    public static final String SQL_CTEATE_VIEW_ERROR_MSG = "Fail to create view in Postgres database";


    private String rdbUrl = null; 
	private String rdbUser = null;
	private String rdbPassword = null;
    private Connection conn = null;
    private DSLContext context;
    private static final SQLDialect dialect = SQLDialect.POSTGRES;
    private static final String tableName = "trafficincident";
    private static final Field<String> iriColumn = DSL.field(DSL.name("iri"), String.class);
    private static final Field<Long> startTimeColumn = DSL.field(DSL.name("start_time"), Long.class);
    private static final Field<Long> endTimeColumn = DSL.field(DSL.name("end_time"), Long.class);
    private static final Field<String> typeColumn = DSL.field(DSL.name("type"), String.class);
    private static final Field<Double> latitudeColumn = DSL.field(DSL.name("latitude"), double.class);
    private static final Field<Double> longitudeColumn = DSL.field(DSL.name("longitude"), double.class);
    private static final Field<String> messageColumn = DSL.field(DSL.name("message"), String.class);
    private static final Field<Boolean> statusColumn = DSL.field(DSL.name("status"), Boolean.class);

    public TrafficIncidentPostgresAgent(String rdbUrl, String rdbUser, String rdbPassword) {
        this.rdbUrl = rdbUrl;
        this.rdbUser = rdbUser;
        this.rdbPassword = rdbPassword;
    }

    protected void connect() {
        try {
            if (this.conn == null || this.conn.isClosed()) {
                // Load required driver
                Class.forName("org.postgresql.Driver");
                // Connect to DB (using static connection and context properties)
                this.conn = DriverManager.getConnection(this.rdbUrl, this.rdbUser, this.rdbPassword);
                this.context = DSL.using(this.conn, dialect);
                LOGGER.debug("Connecting successful: " + this.rdbUrl); 
            }
        } catch (SQLException | ClassNotFoundException e) {
            LOGGER.error(e.getMessage());
            LOGGER.debug("Connecting failed: " + this.rdbUrl);
            throw new JPSRuntimeException("Establishing database connection failed");
        }
    }

    /**
     * Creates schema of specified table name, enables the postgis extension and adds Geometry column
     */
    public void createSchemaIfNotExists() {
        // note that column name will be automatically converted to lowercase
        String createTableSql = "CREATE TABLE IF NOT EXISTS " + this.tableName + " ( iri character varying NOT NULL PRIMARY KEY, start_time bigint NOT NULL, end_time bigint NOT NULL, type character varying NOT NULL, message character varying NOT NULL, latitude double precision NOT NULL, longitude double precision NOT NULL, status boolean DEFAULT false NOT NULL)";
        String enablePostgisSQL = "CREATE EXTENSION IF NOT EXISTS postgis;";
        String alterTableSql = "ALTER TABLE " + this.tableName + " ADD COLUMN IF NOT EXISTS geom GEOMETRY(point, 4326)";
        try {
            PreparedStatement statement = this.conn.prepareStatement(createTableSql);
            // LOGGER.debug(statement);
            statement.execute();
            statement = this.conn.prepareStatement(enablePostgisSQL);
            // LOGGER.debug(statement);
            statement.execute();
            statement = this.conn.prepareStatement(alterTableSql);
            // LOGGER.debug(statement);
            statement.execute();
        } catch (SQLException e) {
            LOGGER.error(SQL_INITIALIZE_ERROR_MSG, e);
            throw new JPSRuntimeException(e.getMessage());
        }
    }

    /**
     * Retrieves ongoing incidents from Postgres and @return a HashSet of these incidents
     * Whether incidents are ongoing is decided by its "status" field in Postgres
     */
    public HashSet<TrafficIncident> retrieveOngoingIncidents() {
        String sql = "SELECT * FROM trafficincident WHERE \"status\" = \'t\'";
        HashSet<TrafficIncident> ongoingTrafficIncidentSet = new HashSet<>();
        ResultSet rs;
        try {
            PreparedStatement statement = this.conn.prepareStatement(sql);
            rs = statement.executeQuery();
            while (rs.next()) {
                String iri = rs.getString("iri");
                String type = rs.getString("type");
                Long startTime = rs.getLong("start_time");
                Long endTime = rs.getLong("end_time");
                Double latitude = rs.getDouble("latitude");
                Double longitude = rs.getDouble("longitude");
                String message = rs.getString("message");
                Boolean status = rs.getBoolean("status");
                TrafficIncident curr = new TrafficIncident(iri, type, latitude, longitude, message, startTime, status);
                ongoingTrafficIncidentSet.add(curr);
            }
        } catch (SQLException e) {
            LOGGER.error(SQL_UPDATE_ERROR_MSG, e);
            throw new JPSRuntimeException(e.getMessage());
        }
        
        return ongoingTrafficIncidentSet;
    }

        /**
     * Inserts the given @param trafficIncident into Postgres
     */
    protected void insertValuesIntoPostgres(TrafficIncident trafficIncident) {
        Table<?> table = DSL.table(DSL.name("trafficincident"));
        InsertValuesStepN<?> insertValueStep = (InsertValuesStepN<?>) context.insertInto(table, iriColumn, startTimeColumn, endTimeColumn, typeColumn, latitudeColumn, longitudeColumn, messageColumn, statusColumn);
        insertValueStep = insertValueStep.values(trafficIncident.iri, trafficIncident.startTime, trafficIncident.endTime, trafficIncident.incidentType, 
            trafficIncident.latitude, trafficIncident.longitude, trafficIncident.message, trafficIncident.status);

        insertValueStep.execute();
    }

    /**
     * updates the end time and status of the given @param trafficIncident
     */
    public void updateTrafficIncidentEndTimeStatusPostgres(TrafficIncident trafficIncident) {
        String sql = "UPDATE trafficincident SET end_time = ?, status = ? WHERE type = ? and start_time = ? and latitude = ? and longitude = ?";
        try {
            PreparedStatement statement = this.conn.prepareStatement(sql);
            statement.setLong(1, trafficIncident.endTime);
            statement.setBoolean(2, trafficIncident.status);
            statement.setString(3, trafficIncident.incidentType);
            statement.setLong(4, trafficIncident.startTime);
            statement.setDouble(5, trafficIncident.latitude);
            statement.setDouble(6, trafficIncident.longitude);
            // LOGGER.debug(statement);
            int rowAffected = statement.executeUpdate();
        } catch (SQLException e) {
            LOGGER.error(SQL_UPDATE_ERROR_MSG, e);
            throw new JPSRuntimeException(e.getMessage());
        }
    }

    /**
     * Adds geom field for all records in TrafficIncident table without geom based on longitude latitude column
     */
    public void convertLongLatPairToGeom() {
        // WSG4326 coordinates used in this case
        String sql = "UPDATE trafficincident SET geom = ST_SETSRID(ST_MakePoint(trafficincident.longitude, trafficincident.latitude), 4326) WHERE geom IS NULL";
        try {
            PreparedStatement statement = this.conn.prepareStatement(sql);
            statement.executeUpdate();
        } catch (SQLException e) {
            LOGGER.error(SQL_UPDATE_ERROR_MSG, e);
            throw new JPSRuntimeException(e.getMessage());
        }
    }

    protected void disconnect() {
		try {
			conn.close();
			LOGGER.debug("Disconnecting successful"); 
		} catch (SQLException e) {
			LOGGER.error(e.getMessage());
			LOGGER.debug("Disconnecting failed");
			throw new JPSRuntimeException("Closing database connection failed");
		}
	}
}