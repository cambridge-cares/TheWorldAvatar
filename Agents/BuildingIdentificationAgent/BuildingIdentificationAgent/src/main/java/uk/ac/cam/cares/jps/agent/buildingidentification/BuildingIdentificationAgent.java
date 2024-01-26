package uk.ac.cam.cares.jps.agent.buildingidentification;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jooq.DSLContext;
import org.jooq.impl.DSL;
import org.jooq.SQLDialect;

import java.sql.*;
import java.util.*;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

@WebServlet(urlPatterns = { BuildingIdentificationAgent.URI_ARRAY,
        BuildingIdentificationAgent.URI_TABLE
})
public class BuildingIdentificationAgent extends JPSAgent {

    public static final String KEY_REQ_URL = "requestUrl";
    public static final String URI_ARRAY = "/array";
    public static final String URI_TABLE = "/table";
    public static final String KEY_DISTANCE = "maxDistance";
    public static final String KEY_COORD = "coordinates";
    public static final String KEY_SRID = "srid";
    public static final String KEY_TABLE = "table";
    public static final String KEY_SCHEMA = "schema";
    private static final String EPSG = "EPSG:";

    private static final Logger LOGGER = LogManager.getLogger(BuildingIdentificationAgent.class);

    // RDBStore client to query postgis
    private RemoteRDBStoreClient rdbStoreClient;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {

            // Properties of database containing buildings data.
            String dbUrl = null;
            String dbUser = null;
            String dbPassword = null;
            String dbName = "postgres";

            if (requestParams.has("dbUrl")) {
                // for local testing outside stack
                dbUrl = requestParams.getString("dbUrl");
                dbUser = requestParams.getString("dbUser");
                dbPassword = requestParams.getString("dbPassword");
            } else {
                EndpointConfig endpointConfig = new EndpointConfig();
                dbUrl = endpointConfig.getDbUrl(dbName);
                dbUser = endpointConfig.getDbUser();
                dbPassword = endpointConfig.getDbPassword();
            }

            rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);
            int dbSrid = getDbSrid();

            // Maximum distance for matching. If the distance to the centroid of the nearest
            // building footprint is larger than this value for a particular point, a
            // warning is printed to the Docker logs.
            double maxDistance = 100.0;

            if (requestParams.has(KEY_DISTANCE))
                maxDistance = Double.parseDouble(requestParams.getString(KEY_DISTANCE));

            // SRID of input coordinates. This is assumed to be EPSG:4326 if not specified
            // in the incoming request. Only applicable for array route.
            int inputSrid = 4326;
            if (requestParams.has(KEY_SRID))
                inputSrid = Integer.parseInt(requestParams.getString(KEY_SRID));

            // Reset all variables
            // List of coordinates for which the nearest building needs to be identified
            List<List<Double>> locations = new ArrayList<>();
            // Number of buildings matched. Should be equal to locations.size().
            int numberBuildingsIdentified = 0;

            JSONObject responseObject = new JSONObject();

            if (requestParams.getString(KEY_REQ_URL).contains(URI_ARRAY)) {

                JSONArray coords = requestParams.getJSONArray(KEY_COORD);

                for (int i = 0; i < coords.length(); i++) {
                    JSONArray point = coords.getJSONArray(i);
                    double[] xyOriginal = { point.getDouble(0), point.getDouble(1) };
                    double[] xyTransformed = CRSTransformer.transform(EPSG + inputSrid, EPSG + dbSrid, xyOriginal);
                    List<Double> pointLocation = Arrays.asList(xyTransformed[0], xyTransformed[1]);
                    locations.add(pointLocation);
                }
                List<Integer> buildings = linkBuildingsArray(dbSrid, maxDistance, locations);
                responseObject.put("building_id", new JSONArray(buildings));
                numberBuildingsIdentified = buildings.size();
            } else if (requestParams.getString(KEY_REQ_URL).contains(URI_TABLE)) {
                String tableName = requestParams.getString(KEY_TABLE);
                Map<Integer, Integer> buildings = linkBuildingsTable(maxDistance, tableName);
                numberBuildingsIdentified = buildings.size();
                updateBuildings(buildings, tableName);
                responseObject.put("building_id", new JSONArray(buildings.values()));
            }

            responseObject.put("number_matched", numberBuildingsIdentified);
            return responseObject;
        } else
            throw new BadRequestException("Invalid input.");
    }

    /**
     * Checks validity of incoming request
     * 
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {

        if (!requestParams.has(KEY_COORD) && !requestParams.has(KEY_TABLE)) {
            LOGGER.error("The value of either the coordinates or the table parameter must be specified.");
            return false;
        }

        if (requestParams.has(KEY_COORD)) {
            JSONArray coordinates = requestParams.getJSONArray(KEY_COORD);
            for (int i = 0; i < coordinates.length(); i++) {
                JSONArray pointLocation = coordinates.getJSONArray(i);
                if (!isNumber(String.valueOf(pointLocation.get(0)))
                        || !isNumber(String.valueOf(pointLocation.get(1)))) {
                    LOGGER.error(
                            "The coordinates specified for index {} of the input array cannot be parsed as a number.",
                            i);
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Queries database for the SRID
     * 
     */
    private int getDbSrid() {

        int dbSrid = -1;
        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            String sqlString = "SELECT srid,gml_srs_name from citydb.database_srs";
            ResultSet result = stmt.executeQuery(sqlString);
            if (result.next()) {
                dbSrid = result.getInt("srid");
            } else {
                LOGGER.warn("Could not retrieve srid from database.");
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
        return dbSrid;
    }

    /**
     * Identifies the building whose envelope centroid is closest to the coordinates
     * of each point in the user-specified JSONArray.
     * 
     * @param dbSrid: SRID used to store buildings data in citydb schema
     * 
     * @return None
     */

    private List<Integer> linkBuildingsArray(int dbSrid, double maxDistance, List<List<Double>> locations) {

        List<Integer> buildings = new ArrayList<>();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            for (int i = 0; i < locations.size(); i++) {
                List<Double> loc = locations.get(i);
                String sqlString = String.format("select id, wkt, dist from ( " +
                        "select cityobject.id as id, public.ST_AsText(envelope) as wkt, "
                        +
                        "public.ST_DISTANCE(public.ST_Point(%f,%f, %d), envelope) AS dist from citydb.cityobject "
                        +
                        "WHERE cityobject.objectclass_id = 26 " +
                        ") AS sub " +
                        "order by dist " +
                        "limit 1",
                        loc.get(0), loc.get(1), dbSrid);

                ResultSet result = stmt.executeQuery(sqlString);

                while (result.next()) {
                    int buildingId = result.getInt("id");
                    buildings.add(buildingId);

                    Double dist = result.getDouble("dist");

                    if (dist > maxDistance)
                        LOGGER.warn("Nearest footprint for coordinate at index {} is {} meters away",
                                i, dist);
                }

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    private Map<Integer, Integer> linkBuildingsTable(double maxDistance, String tableName) {

        Map<Integer, Integer> buildings = new HashMap<>();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("SELECT ogc_fid, building_id, dist FROM " +
                    "(SELECT ogc_fid, geometry FROM %s) r1 " +
                    "LEFT JOIN LATERAL ( " +
                    " SELECT envelope, citydb.cityobject.id AS building_id, " +
                    " public.ST_DISTANCE(public.ST_TRANSFORM(r1.geometry, public.ST_SRID(citydb.cityobject.envelope)), citydb.cityobject.envelope) AS dist "
                    +
                    " FROM citydb.cityobject " +
                    " where citydb.cityobject.objectclass_id = 26 " +
                    " ORDER BY dist " +
                    " LIMIT 1 " +
                    " ) r2 ON true;", tableName);
            ResultSet result = stmt.executeQuery(sqlString);

            while (result.next()) {
                int ogcFid = result.getInt("ogc_fid");
                int buildingId = result.getInt("building_id");
                buildings.put(ogcFid, buildingId);
                Double dist = result.getDouble("dist");

                if (dist > maxDistance)
                    LOGGER.warn(" The building with id {} has been matched to a point {} meters away",
                            buildingId, dist);

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    public void updateBuildings(Map<Integer, Integer> buildings, String tableName) {

        String columnName = "building_id";

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("ALTER TABLE %s " +
                    "DROP COLUMN IF EXISTS %s ", tableName, columnName);
            stmt.executeUpdate(sqlString);

            sqlString = String.format("ALTER TABLE %s " +
                    " ADD COLUMN %s int ", tableName, columnName);

            stmt.executeUpdate(sqlString);

            sqlString = String.format(" UPDATE %s SET %s  = CASE  ", tableName, columnName);

            StringBuilder update = new StringBuilder(sqlString);

            update.append(System.lineSeparator());

            for (Map.Entry<Integer, Integer> entry : buildings.entrySet()) {
                String val = String.format("WHEN ogc_fid = %d THEN %d ", entry.getKey(), entry.getValue());
                update.append(val);
                update.append(System.lineSeparator());
            }

            update.append("END;");

            stmt.executeUpdate(update.toString());

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    /**
     * Checks if a string is able to be parsable as a number
     * 
     * @param number string to check
     * @return boolean value of check
     */
    public boolean isNumber(String number) {
        try {
            Double.parseDouble(number);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    DSLContext getContext(Connection conn) {
        return DSL.using(conn, SQLDialect.POSTGRES);
    }

    boolean tableExists(String tableName, Connection conn) {
        String condition = String.format("table_name = '%s'", tableName);
        return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
                int.class) == 1;
    }

    boolean columnExists(String tableName, String columnName, Connection conn) {
        String condition = String.format("table_name = '%s' AND column_name = '%s'", tableName, columnName);
        return getContext(conn).select(DSL.count()).from("information_schema.columns").where(condition).fetchOne(0,
                int.class) == 1;
    }

}