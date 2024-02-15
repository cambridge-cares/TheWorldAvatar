package uk.ac.cam.cares.jps.agent.buildingidentification;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.sql.*;
import java.util.*;
import uk.ac.cam.cares.jps.base.util.CRSTransformer;

@WebServlet(urlPatterns = { BuildingIdentificationAgent.ROUTE_LOCATION,
        BuildingIdentificationAgent.ROUTE_POSTGIS
})
public class BuildingIdentificationAgent extends JPSAgent {

    public static final String KEY_REQ_URL = "requestUrl";
    public static final String ROUTE_LOCATION = "/location";
    public static final String ROUTE_POSTGIS = "/postgis";
    public static final String KEY_DISTANCE = "maxDistance";
    public static final String KEY_COORD = "coordinates";
    public static final String KEY_SRID = "srid";
    public static final String KEY_TABLE = "table";
    public static final String KEY_COLUMN = "column";
    public static final String KEY_SCHEMA = "schema";
    public static final String KEY_IRI_PREFIX = "iriPrefix";
    public static final String KEY_ONE_MANY = "oneToMany";
    public static final String KEY_NEW_TABLE = "newTable";
    public static final String KEY_FILTER_COLUMNS = "filterColumns";
    public static final String KEY_EXCLUDED_VALUES = "excludedValues";
    private static final String EPSG = "EPSG:";
    public static final String POINT_TYPE = "ST_Point";

    private static final Logger LOGGER = LogManager.getLogger(BuildingIdentificationAgent.class);

    private static final String COLUMN_NAME = "building_iri";

    // RDBStore client to query postgis
    private RemoteRDBStoreClient rdbStoreClient;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {

            // Properties of database containing buildings data.
            String dbUrl = null;
            String dbUser = null;
            String dbPassword = null;

            if (requestParams.has("dbUrl")) {
                dbUrl = requestParams.getString("dbUrl");
                dbUser = requestParams.getString("dbUser");
                dbPassword = requestParams.getString("dbPassword");
            } else {
                String dbName = "postgres";
                EndpointConfig endpointConfig = new EndpointConfig();
                dbUrl = endpointConfig.getDbUrl(dbName);
                dbUser = endpointConfig.getDbUser();
                dbPassword = endpointConfig.getDbPassword();
            }

            rdbStoreClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);
            int dbSrid = getDbSrid();

            // Parse optional parameters

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

            // Name of column containing user-specified geometries. Only applicable for
            // postgis route.
            String columnName = "geometry";
            if (requestParams.has(KEY_COLUMN))
                columnName = requestParams.getString(KEY_COLUMN);

            // Boolean variable which is true if there are multiple buildings associated
            // with
            // each user-specified geometry
            boolean oneToMany = false;
            String newTable = "matched_buildings";
            if (requestParams.has(KEY_ONE_MANY)) {
                oneToMany = Boolean.parseBoolean(requestParams.getString(KEY_ONE_MANY));
                if (requestParams.has(KEY_NEW_TABLE))
                    newTable = requestParams.getString(KEY_NEW_TABLE);
            }

            // Reset all variables
            // List of coordinates for which the nearest building needs to be identified
            List<List<Double>> locations = new ArrayList<>();
            // Number of buildings matched. Should be equal to locations.size().
            int numberBuildingsIdentified = 0;

            JSONObject responseObject = new JSONObject();

            if (requestParams.getString(KEY_REQ_URL).contains(ROUTE_LOCATION)) {

                JSONArray coords = requestParams.getJSONArray(KEY_COORD);

                for (int i = 0; i < coords.length(); i++) {
                    JSONArray point = coords.getJSONArray(i);
                    double[] xyOriginal = { point.getDouble(0), point.getDouble(1) };
                    double[] xyTransformed = CRSTransformer.transform(EPSG + inputSrid, EPSG + dbSrid, xyOriginal);
                    List<Double> pointLocation = Arrays.asList(xyTransformed[0], xyTransformed[1]);
                    locations.add(pointLocation);
                }

                LOGGER.info("{}, {}", locations.get(0).get(0), locations.get(0).get(1));
                List<String> buildings = linkBuildingsArray(dbSrid, maxDistance, locations);
                responseObject.put(COLUMN_NAME, new JSONArray(buildings));
                numberBuildingsIdentified = buildings.size();
            } else if (requestParams.getString(KEY_REQ_URL).contains(ROUTE_POSTGIS)) {
                String tableName = requestParams.getString(KEY_TABLE);

                if (oneToMany) {
                    JSONArray filterColumns = requestParams.getJSONArray(KEY_FILTER_COLUMNS);
                    JSONArray excludedValues = requestParams.getJSONArray(KEY_EXCLUDED_VALUES);

                    Map<Integer, List<String>> buildings = linkMultipleBuildings(tableName, columnName, dbSrid,
                            filterColumns, excludedValues);
                    numberBuildingsIdentified = buildings.values().stream().mapToInt(p -> p.size()).sum();
                    updateMultipleBuildings(tableName, newTable, buildings);
                } else {
                    String geomType = getGeometryType(tableName, columnName);
                    Map<Integer, String> buildings;
                    if (geomType.equals(POINT_TYPE))
                        buildings = linkBuildingsTable(maxDistance, tableName, columnName, dbSrid);
                    else
                        buildings = linkBuildingsTableNonPoint(maxDistance, tableName, columnName, dbSrid);
                    numberBuildingsIdentified = buildings.size();
                    updateBuildings(buildings, tableName);
                }

            } else {
                String route = requestParams.getString(KEY_REQ_URL);
                LOGGER.fatal("{}{}", "The Building Identification Agent does not support the route ", route);
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

    private String getGeometryType(String tableName, String columnName) {

        String geomType = POINT_TYPE;

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            String sqlString = String.format("SELECT ST_GEOMETRYTYPE(%s) as geom_type from %s limit 1;", columnName,
                    tableName);
            ResultSet result = stmt.executeQuery(sqlString);
            if (result.next()) {
                geomType = result.getString("geom_type");
            } else {
                LOGGER.warn("Could not retrieve type of user-specified geometries.");
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return geomType;
    }

    /**
     * Identifies the building whose envelope centroid is closest to the coordinates
     * of each point in the user-specified JSONArray.
     * 
     * @param dbSrid: SRID used to store buildings data in citydb schema
     * 
     * @return None
     * 
     *         For this function to work efficiently for large numbers of points,
     *         one must create a spatial index on envelope and
     *         ST_Boundary(envelope)
     *         as follows:
     * 
     *         create index "envelope_boundary" on citydb.cityobject
     *         using gist(envelope, public.ST_BOUNDARY(envelope)) ;
     * 
     *         Also, the <-> operator should be used to find nearest neighbours.
     *         Using ST_DISTANCE causes POSTGIS to execute a sequential scan
     *         on the envelope column of cityobject, which can increase the running
     *         time by a few orders of magnitude. See
     *         https://www.crunchydata.com/blog/a-deep-dive-into-postgis-nearest-neighbor-search
     * 
     * 
     */

    private List<String> linkBuildingsArray(int dbSrid, double maxDistance, List<List<Double>> locations) {

        List<String> buildings = new ArrayList<>();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlTemplate = "select cityobject_genericattrib.strval as iri, "
                    +
                    "public.ST_Point(%f, %f, %d) <-> envelope AS dist," +

                    "public.ST_Point(%f, %f, %d) <-> public.ST_Boundary(envelope) AS distBoundary " +

                    " from citydb.cityobject, citydb.cityobject_genericattrib "
                    +

                    " AND \"cityobject_genericattrib\".\"attrname\" = 'uuid' " +

                    " AND cityobject.objectclass_id = 26 " +

                    " AND cityobject.id = cityobject_genericattrib.cityobject_id "
                    +
                    " order by dist, distBoundary " +
                    " limit 1;";

            for (int i = 0; i < locations.size(); i++) {
                List<Double> loc = locations.get(i);
                String sqlString = String.format(sqlTemplate,
                        loc.get(0), loc.get(1), dbSrid);

                ResultSet result = stmt.executeQuery(sqlString);

                while (result.next()) {
                    String buildingId = result.getString("iri");
                    buildings.add(buildingId);

                    Double dist = result.getDouble("dist");

                    if (dist > maxDistance)
                        LOGGER.warn(" The building with UUID {} has been matched to a point {} meters away",
                                buildingId, dist);
                }

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    private Map<Integer, String> linkBuildingsTable(double maxDistance, String tableName, String columnName,
            Integer srid) {

        Map<Integer, String> buildings = new HashMap<>();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("SELECT ogc_fid, iri, dist FROM " +
                    "(SELECT ogc_fid, public.ST_TRANSFORM(\"%s\", %d) AS geometry FROM %s) r1 " +
                    "LEFT JOIN LATERAL ( " +
                    " SELECT cityobject_genericattrib.strval AS iri, " +
                    " r1.geometry <-> cityobject.envelope AS dist, " +
                    " r1.geometry <-> ST_Boundary(cityobject.envelope) AS distBoundary " +
                    " FROM citydb.cityobject, citydb.cityobject_genericattrib " +
                    " where cityobject_genericattrib.attrname = 'uuid' " +
                    " AND cityobject.objectclass_id = 26 " +
                    " AND cityobject.id = cityobject_genericattrib.cityobject_id" +
                    " ORDER BY dist, distBoundary " +
                    " LIMIT 1 " +
                    " ) r2 ON true;", columnName, srid, tableName);
            ResultSet result = stmt.executeQuery(sqlString);

            while (result.next()) {
                int ogcFid = result.getInt("ogc_fid");
                String buildingId = result.getString("iri");
                buildings.put(ogcFid, buildingId);
                Double dist = result.getDouble("dist");

                if (dist > maxDistance)
                    LOGGER.warn(" The building with UUID {} has been matched to a point {} meters away",
                            buildingId, dist);

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    private Map<Integer, String> linkBuildingsTableNonPoint(double maxDistance, String tableName, String columnName,
            Integer srid) {

        Map<Integer, String> buildings = new HashMap<>();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("SELECT ogc_fid, iri, dist FROM " +
                    "(SELECT ogc_fid, public.ST_TRANSFORM(\"%s\", %d) AS geometry FROM %s) r1 " +
                    "LEFT JOIN ( " +
                    " SELECT cityobject_genericattrib.strval AS iri, " +
                    " envelope AS footprint " +
                    " FROM citydb.cityobject, citydb.cityobject_genericattrib " +
                    " WHERE cityobject_genericattrib.attrname = 'uuid' " +
                    " AND cityobject.objectclass_id = 26 " +
                    " AND cityobject.id = cityobject_genericattrib.cityobject_id" +
                    " ) r2 ON public.ST_Within(r1.geometry, r2.footprint) ;",
                    columnName, srid, tableName);
            ResultSet result = stmt.executeQuery(sqlString);

            while (result.next()) {
                int ogcFid = result.getInt("ogc_fid");
                String buildingId = result.getString("iri");
                buildings.put(ogcFid, buildingId);
                Double dist = result.getDouble("dist");

                if (dist > maxDistance)
                    LOGGER.warn(" The building with UUID {} has been matched to a geometry {} meters away",
                            buildingId, dist);

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    private Map<Integer, List<String>> linkMultipleBuildings(String tableName, String columnName,
            Integer srid, JSONArray filterColumns, JSONArray excludedValues) {

        Map<Integer, List<String>> buildings = new HashMap<>();

        String sqlTemplate = String.format("SELECT ogc_fid, iri FROM " +
                " ( SELECT cityobject_genericattrib.strval AS iri, " +
                " envelope AS footprint " +
                " FROM citydb.cityobject, " +
                " citydb.cityobject_genericattrib " +
                " WHERE cityobject.objectclass_id = 26 " +
                " AND cityobject.id = cityobject_genericattrib.cityobject_id " +
                " AND cityobject_genericattrib.attrname = 'uuid') r2 " +
                " LEFT JOIN LATERAL " +
                " (SELECT ogc_fid, " +
                " r2.footprint <-> public.ST_TRANSFORM(\"%s\", %d) AS dist " +
                " FROM %s ", columnName, srid, tableName);
        StringBuilder sqlBuilder = new StringBuilder(sqlTemplate);
        sqlBuilder.append(System.lineSeparator());

        for (int i = 0; i < filterColumns.length(); i++) {

            StringBuilder filterString = new StringBuilder(String.format(" AND \"%s\" NOT IN (", filterColumns.get(i)));
            JSONArray removedValues = excludedValues.getJSONArray(i);
            for (int j = 0; j < removedValues.length(); j++) {
                String columnValue = String.format(",'%s'", removedValues.getString(j));
                if (j == 0)
                    columnValue = columnValue.replace(",", "");
                filterString.append(columnValue);
            }
            filterString.append(") ");
            String filterFinal = filterString.toString();
            if (i == 0)
                filterFinal = filterFinal.replace("AND", "WHERE");
            sqlBuilder.append(filterFinal);
        }

        sqlBuilder.append(System.lineSeparator());
        sqlBuilder.append(" order by dist ")
                .append(" limit 1) r1 on true;");

        String sqlString = sqlBuilder.toString();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            ResultSet result = stmt.executeQuery(sqlString);

            while (result.next()) {
                int ogcFid = result.getInt("ogc_fid");
                String buildingIri = result.getString("iri");
                if (buildings.containsKey(ogcFid)) {
                    buildings.get(ogcFid).add(buildingIri);
                } else {
                    List<String> iriList = new ArrayList<>(Arrays.asList(buildingIri));
                    buildings.put(ogcFid, iriList);
                }

            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return buildings;

    }

    public void updateBuildings(Map<Integer, String> buildings, String tableName) {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("ALTER TABLE %s " +
                    "DROP COLUMN IF EXISTS %s ", tableName, COLUMN_NAME);
            stmt.executeUpdate(sqlString);

            sqlString = String.format("ALTER TABLE %s " +
                    " ADD COLUMN %s character varying (10000) ", tableName, COLUMN_NAME);

            stmt.executeUpdate(sqlString);

            sqlString = String.format(" UPDATE %s SET %s  = CASE  ", tableName, COLUMN_NAME);

            StringBuilder update = new StringBuilder(sqlString);

            update.append(System.lineSeparator());

            for (Map.Entry<Integer, String> entry : buildings.entrySet()) {
                String val = String.format("WHEN ogc_fid = %d THEN '%s' ", entry.getKey(), entry.getValue());
                update.append(val);
                update.append(System.lineSeparator());
            }

            update.append("END;");

            stmt.executeUpdate(update.toString());

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    public void updateMultipleBuildings(String userTable, String tableName, Map<Integer, List<String>> buildings) {

        userTable = userTable.replace(".", "_");

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format(" DROP TABLE IF EXISTS %s ;", tableName);
            stmt.executeUpdate(sqlString);

            sqlString = String.format("CREATE TABLE %s (" +
                    " \"%s_ogc_fid\" bigint NOT NULL, " +
                    " \"%s\" character varying (10000) ) ;", tableName, userTable, COLUMN_NAME);

            stmt.executeUpdate(sqlString);

            sqlString = String.format(" INSERT INTO %s (\"%s_ogc_fid\", \"%s\")  VALUES  ", tableName, userTable,
                    COLUMN_NAME);

            StringBuilder update = new StringBuilder(sqlString);

            update.append(System.lineSeparator());

            for (Map.Entry<Integer, List<String>> entry : buildings.entrySet()) {
                List<String> iriList = entry.getValue();
                iriList.stream().forEach(iri -> {
                    String val = String.format("(%d, '%s'),", entry.getKey(), iri);
                    update.append(val);
                    update.append(System.lineSeparator());

                });
            }

            String updateString = update.toString();
            int lastIndex = updateString.lastIndexOf(",");
            String finalString = updateString.substring(0, lastIndex);
            finalString = finalString.concat(";");
            stmt.executeUpdate(finalString);

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

}