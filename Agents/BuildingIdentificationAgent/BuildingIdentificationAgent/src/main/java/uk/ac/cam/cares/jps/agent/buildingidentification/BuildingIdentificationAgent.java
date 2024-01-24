package uk.ac.cam.cares.jps.agent.buildingidentification;

import javax.servlet.annotation.WebServlet;
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
    private static final String epsg = "EPSG:";

    private static final Logger LOGGER = LogManager.getLogger(BuildingIdentificationAgent.class);

    // Properties of database containing buildings data.
    private String dbUrl = null;
    private String dbUser = null;
    private String dbPassword = null;
    private String dbName = "postgres";
    private int dbSrid;
    // RDBStore client to query postgis
    private RemoteRDBStoreClient rdbStoreClient;

    // SRID of input coordinates. This is assumed to be EPSG:4326 if not specified
    // in the incoming request.
    private int inputSrid = 4326;
    // Maximum distance for matching. If the distance to the centroid of the nearest
    // building footprint is larger than this value for a particular point, a
    // warning is printed to the Docker logs.
    private double maxDistance = 100.0;
    // Number of buildings matched. Should be equal to locations.size().
    private int numberBuildingsIdentified = 0;
    // List of coordinates for which the nearest building needs to be identified
    private List<List<Double>> locations = new ArrayList<>();
    // JSONArray containing building IDs that is returned by the agent
    private List<Integer> buildings = new ArrayList<>();

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {

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
            getDbSrid();

            if (requestParams.has(KEY_DISTANCE))
                maxDistance = Double.parseDouble(requestParams.getString(KEY_DISTANCE));

            if (requestParams.has(KEY_SRID))
                inputSrid = Integer.parseInt(requestParams.getString(KEY_SRID));

            // Reset all variables
            locations.clear();
            buildings.clear();
            numberBuildingsIdentified = 0;

            JSONArray coords = requestParams.getJSONArray(KEY_COORD);

            for (int i = 0; i < coords.length(); i++) {
                JSONArray point = coords.getJSONArray(i);
                double[] xyOriginal = { point.getDouble(0), point.getDouble(1) };
                double[] xyTransformed = CRSTransformer.transform(epsg + inputSrid, epsg + dbSrid, xyOriginal);
                List<Double> pointLocation = Arrays.asList(xyTransformed[0], xyTransformed[1]);
                locations.add(pointLocation);
            }

            linkBuildings();
        }

        JSONObject responseObject = new JSONObject();
        responseObject.put("building_id", new JSONArray(buildings));
        responseObject.put("number_matched", numberBuildingsIdentified);
        return responseObject;
    }

    /**
     * Checks validity of incoming request
     * 
     * @param requestParams Request parameters as JSONObject
     * @return Validity of request
     */
    @Override
    public boolean validateInput(JSONObject requestParams) {

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
        } else if (requestParams.has(KEY_TABLE)) {
            // Check if table exists
            String table = requestParams.getString(KEY_TABLE);

            try (Connection conn = rdbStoreClient.getConnection();) {
                if (!tableExists(table, conn)) {
                    LOGGER.error("The specified PostgreSQL table {} does not exist", table);
                }
                return false;
            } catch (SQLException e) {
                LOGGER.error(e.getMessage());
                return false;
            }
        } else {
            LOGGER.error("The value of either the coordinates or the table parameter must be specified.");
            return false;
        }

        return true;
    }

    /**
     * Queries database for the SRID
     * 
     */
    private void getDbSrid() {
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
    }

    /**
     * Identifies the building whose envelope centroid is closest to the coordinate
     * of each factory.
     * 
     * 
     * @return None
     */

    private void linkBuildings() {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            for (int i = 0; i < locations.size(); i++) {
                List<Double> loc = locations.get(i);
                String sqlString = String.format("select id, wkt, height, dist from ( " + System.lineSeparator() +
                        "select cityobject.id as id, measured_height as height, public.ST_AsText(envelope) as wkt, "
                        +
                        System.lineSeparator() +
                        "public.ST_DISTANCE(public.ST_Point(%f,%f, %d), envelope) AS dist from citydb.cityobject, citydb.building"
                        +
                        System.lineSeparator() +
                        "WHERE cityobject.objectclass_id = 26 AND cityobject.id = building.id" +
                        System.lineSeparator() +
                        ") AS sub" + System.lineSeparator() +
                        "order by dist" + System.lineSeparator() +
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
                    numberBuildingsIdentified++;
                }

            }

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

}