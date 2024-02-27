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
import org.jooq.SQLDialect;
import org.jooq.exception.DataAccessException;
import org.jooq.impl.DSL;

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
    public static final String KEY_ONE_MANY = "oneToMany";
    public static final String KEY_NEW_TABLE = "newTable";
    public static final String KEY_FILTER_COLUMNS = "filterColumns";
    public static final String KEY_EXCLUDED_VALUES = "excludedValues";
    public static final String KEY_OVERLAP_FRACTION = "overlapFraction";
    private static final String EPSG = "EPSG:";
    public static final String POINT_TYPE = "ST_Point";

    private static final Logger LOGGER = LogManager.getLogger(BuildingIdentificationAgent.class);

    private static final String BUILDINGS_COLUMN = "building_uuid";

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

            // Create table containing buildings footprints data if it does not exist.
            getBuildingFootprints();

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

            // Minimum overlap fraction
            Double overlapFraction = 0.70;
            if (requestParams.has(KEY_OVERLAP_FRACTION))
                overlapFraction = requestParams.getDouble(KEY_OVERLAP_FRACTION);

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
                responseObject.put(BUILDINGS_COLUMN, new JSONArray(buildings));
                numberBuildingsIdentified = buildings.size();
            } else if (requestParams.getString(KEY_REQ_URL).contains(ROUTE_POSTGIS)) {
                String tableName = requestParams.getString(KEY_TABLE);

                if (oneToMany) {
                    JSONArray filterColumns = requestParams.getJSONArray(KEY_FILTER_COLUMNS);
                    JSONArray excludedValues = requestParams.getJSONArray(KEY_EXCLUDED_VALUES);

                    linkMultipleBuildings(tableName, columnName, dbSrid,
                            filterColumns, excludedValues, newTable);
                    numberBuildingsIdentified = getNumberOfMatchedBuildings(newTable);

                } else {
                    String geomType = getGeometryType(tableName, columnName);
                    if (geomType.equals(POINT_TYPE))
                        linkBuildingsTablePoint(maxDistance, tableName, columnName, dbSrid);
                    else
                        linkBuildingsTableNonPoint(tableName, columnName, dbSrid,
                                overlapFraction);
                    numberBuildingsIdentified = getNumberOfMatchedBuildings(tableName);
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

    void getBuildingFootprints() {
        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            if (!checkTableExists("building_footprints", conn)) {
                String sqlString = " drop table if exists building_footprints; " +
                        " create table building_footprints AS ( " +
                        " SELECT b.id AS ogc_fid, cga.strval AS uuid, ST_Collect(sg.geometry) as footprint_geometry, " +
                        " MAX(b.measured_height) AS height " +
                        " FROM citydb.building b " +
                        " INNER JOIN citydb.cityobject_genericattrib cga ON b.id = cga.cityobject_id " +
                        " INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.parent_id " +
                        "  WHERE cga.attrname = 'uuid' AND sg.geometry IS NOT NULL " +
                        "  GROUP BY b.id, cga.strval ) ; " +
                        "  CREATE INDEX building_footprint_index ON building_footprints USING gist (footprint_geometry) ; ";
                stmt.executeUpdate(sqlString);
            }
        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }
    }

    private String getGeometryType(String tableName, String columnName) {

        String geomType = POINT_TYPE;

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {
            String sqlString = String.format("SELECT ST_GEOMETRYTYPE(\"%s\") as geom_type from \"%s\" limit 1;",
                    columnName,
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
     * @return list of matchde building UUIDs
     * 
     *         For this function to work efficiently for large numbers of points,
     *         one must create a spatial index on footprint_geometry
     *         as follows:
     * 
     *         create index "building_footprint_index" on building_footprints
     *         using gist(footprint_geometry) ;
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

            String sqlTemplate = "select uuid, "
                    +
                    "public.ST_Point(%f, %f, %d) <-> footprint_geometry AS dist " +

                    " from building_footprints " +
                    " order by dist " +
                    " limit 1;";

            for (int i = 0; i < locations.size(); i++) {
                List<Double> loc = locations.get(i);
                String sqlString = String.format(sqlTemplate,
                        loc.get(0), loc.get(1), dbSrid);

                ResultSet result = stmt.executeQuery(sqlString);

                while (result.next()) {
                    String buildingId = result.getString("uuid");
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

    private void linkBuildingsTablePoint(double maxDistance, String tableName, String columnName,
            Integer srid) {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format(
                    " alter table \"%s\" drop column if exists %s ;" +
                            " alter table \"%s\" add column %s character varying (10000) ; " +
                            " UPDATE \"%s\" set %s = match.uuid from ( " +
                            "SELECT ogc_fid, uuid, dist FROM " +
                            "(SELECT ogc_fid, public.ST_TRANSFORM(\"%s\", %d) AS geometry FROM \"%s\") r1 " +
                            "LEFT JOIN LATERAL ( " +
                            " SELECT uuid, " +
                            " r1.geometry <-> footprint_geometry AS dist " +
                            " FROM building_footprints " +
                            " ORDER BY dist " +
                            " LIMIT 1 " +
                            " ) r2 ON true " +
                            " ) match " +
                            " where \"%s\".ogc_fid = match.ogc_fid " +
                            " AND match.dist < %f ; ",
                    tableName, BUILDINGS_COLUMN,
                    tableName, BUILDINGS_COLUMN,
                    tableName, BUILDINGS_COLUMN,
                    columnName, srid, tableName,
                    tableName,
                    maxDistance);
            stmt.executeUpdate(sqlString);

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    private void linkBuildingsTableNonPoint(String tableName, String columnName,
            Integer srid, double overlapFraction) {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format(

                    " alter table \"%s\" drop column if exists %s ;" +
                            " alter table \"%s\" add column %s character varying (10000) ; " +
                            " With temp_table AS " +
                            " (SELECT case when ST_ISVALID(\"%s\") THEN ST_TRANSFORM(\"%s\", %d) " +
                            " WHEN NOT ST_ISVALID(\"%s\") THEN ST_TRANSFORM(ST_MAKEVALID(\"%s\"), %d) END " +
                            " as geometry, ogc_fid from \"%s\" )" +
                            " UPDATE \"%s\" set %s = match.uuid from ( " +
                            " select uuid, temp_table.ogc_fid as iden from building_footprints, temp_table " +
                            " where ST_INTERSECTS(temp_table.geometry, footprint_geometry) " +
                            " AND ST_AREA(ST_INTERSECTION(footprint_geometry, temp_table.geometry)) >= %f*ST_AREA(temp_table.geometry) ) match "
                            +
                            " where \"%s\".ogc_fid = match.iden ",
                    tableName, BUILDINGS_COLUMN,
                    tableName, BUILDINGS_COLUMN,
                    columnName, columnName, srid,
                    columnName, columnName, srid,
                    tableName,
                    tableName, BUILDINGS_COLUMN,
                    overlapFraction,
                    tableName);
            stmt.executeUpdate(sqlString);

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    private void linkMultipleBuildings(String tableName, String columnName,
            Integer srid, JSONArray filterColumns, JSONArray excludedValues, String newTable) {

        String sqlTemplate = String.format(

                " DROP TABLE IF EXISTS \"%s\" ; " +
                        " CREATE TABLE \"%s\" AS  ( " +
                        "SELECT ogc_fid, \"%s\" FROM " +
                        " ( SELECT uuid AS \"%s\", " +
                        " footprint_geometry AS footprint " +
                        " FROM building_footprints ) r2 " +
                        " LEFT JOIN LATERAL " +
                        " (SELECT ogc_fid, " +
                        " r2.footprint <-> public.ST_TRANSFORM(\"%s\", %d) AS dist " +
                        " FROM \"%s\" ",
                newTable,
                newTable,
                BUILDINGS_COLUMN,
                BUILDINGS_COLUMN,
                columnName, srid,
                tableName);
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
                .append(" limit 1) r1 on true ")
                .append(" ) ; ");

        String sqlString = sqlBuilder.toString();

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            stmt.executeUpdate(sqlString);

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

    }

    private int getNumberOfMatchedBuildings(String tableName) {

        Integer numberMatched = 0;
        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format(" select count(*) as total_matched from \"%s\" WHERE %s IS NOT NULL; ",
                    tableName,
                    BUILDINGS_COLUMN);

            ResultSet result = stmt.executeQuery(sqlString);

            while (result.next()) {
                numberMatched = result.getInt("total_matched");
            }

        } catch (SQLException e) {
            LOGGER.error(e.getMessage());
        }

        return numberMatched;
    }

    boolean checkTableExists(String table, Connection conn) {
        try {
            String condition = String.format("table_name = '%s'", table);
            return getContext(conn).select(DSL.count()).from("information_schema.tables").where(condition).fetchOne(0,
                    int.class) == 1;
        } catch (DataAccessException e) {
            LOGGER.error(e.getMessage());
            throw new RuntimeException(e);
        }
    }

    DSLContext getContext(Connection conn) {
        return DSL.using(conn, SQLDialect.POSTGRES);
    }

    @Deprecated
    public void updateBuildings(Map<Integer, String> buildings, String tableName) {

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format("ALTER TABLE %s " +
                    "DROP COLUMN IF EXISTS %s ", tableName, BUILDINGS_COLUMN);
            stmt.executeUpdate(sqlString);

            sqlString = String.format("ALTER TABLE %s " +
                    " ADD COLUMN %s character varying (10000) ", tableName, BUILDINGS_COLUMN);

            stmt.executeUpdate(sqlString);

            sqlString = String.format(" UPDATE %s SET %s  = CASE  ", tableName, BUILDINGS_COLUMN);

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

    @Deprecated
    public void updateMultipleBuildings(String userTable, String tableName, Map<Integer, List<String>> buildings) {

        userTable = userTable.replace(".", "_");

        try (Connection conn = rdbStoreClient.getConnection();
                Statement stmt = conn.createStatement();) {

            String sqlString = String.format(" DROP TABLE IF EXISTS %s ;", tableName);
            stmt.executeUpdate(sqlString);

            sqlString = String.format("CREATE TABLE %s (" +
                    " \"%s_ogc_fid\" bigint NOT NULL, " +
                    " \"%s\" character varying (10000) ) ;", tableName, userTable, BUILDINGS_COLUMN);

            stmt.executeUpdate(sqlString);

            sqlString = String.format(" INSERT INTO %s (\"%s_ogc_fid\", \"%s\")  VALUES  ", tableName, userTable,
                    BUILDINGS_COLUMN);

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