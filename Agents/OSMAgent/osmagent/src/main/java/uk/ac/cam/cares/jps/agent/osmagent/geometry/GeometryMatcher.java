package uk.ac.cam.cares.jps.agent.osmagent.geometry;

import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

public class GeometryMatcher {
    private static final double threshold = 0.7;

    public static void matchGeometry(String url, String user, String password, String pointTable, String polygonTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(url, user, password);

        int pointSRID;
        int polygonSRID;

        int pointMin;
        int polygonMin;
        int pointMax;
        int polygonMax;

        // get SRID of OSM point geometries
        JSONArray result = rdbStoreClient.executeQuery(sridQuery(pointTable));

        if (!result.isEmpty()) {
            pointSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // get SRID of OSM polygon geometries
        result = rdbStoreClient.executeQuery(sridQuery(polygonTable));

        if (!result.isEmpty()) {
            polygonSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // create temporary tables of CityDB building footprints, transformed to the same SRIDs of OSM point and polygon geometries
        List<String> tables = createTable(rdbStoreClient, pointSRID, polygonSRID);

        String citydbPoint = tables.get(0);
        String citydbPolygon = tables.get(1);

        // get min and max IDs for points table, for chunking
        result = rdbStoreClient.executeQuery(idQuery(pointTable));

        if (!result.isEmpty()) {
            pointMin = result.getJSONObject(0).getInt("min");
            pointMax = result.getJSONObject(0).getInt("max");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // get min and max IDs for polygons table, for chunking
        result = rdbStoreClient.executeQuery(idQuery(polygonTable));

        if (!result.isEmpty()) {
            polygonMin = result.getJSONObject(0).getInt("min");
            polygonMax = result.getJSONObject(0).getInt("max");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // matching osm point geometries with CityDB building footprints
        try (Connection connection = rdbStoreClient.getConnection();
        Statement statement = connection.createStatement()) {
            for (int i = pointMin; i <= pointMax; i++) {
                statement.execute(pointSQL(pointTable, citydbPoint, i));
            }
        }
        catch (SQLException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        try (Connection connection = rdbStoreClient.getConnection();
            Statement statement = connection.createStatement()) {
            for (int i = polygonMin; i <= polygonMax; i++) {
                statement.execute(polygonSQL(polygonTable, citydbPoint, threshold, i));
            }
        }
        catch (SQLException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }

        // delete temporary tables
        deleteTable(rdbStoreClient, citydbPoint, citydbPolygon);
    }


    /**
     * Create tables that store CityDB building footprints, transformed to same SRID of point and polygon geometries
     * @param rdbStoreClient RemoteRDBStoreClient to where the OSM and CityDB data are stored
     * @param pointSRID SRID of OSM point geometries
     * @param polygonSRID SRID of OSM polygon geometries
     * @return
     */
    private static List<String> createTable(RemoteRDBStoreClient rdbStoreClient, int pointSRID, int polygonSRID) {
        String citydbTable = "CREATE TABLE %s AS \n" +
                "WITH rawgeo AS (\n" +
                "SELECT cga.strval AS urival, public.ST_Transform(public.ST_Collect(sg.geometry), %d) as geo, \n" +
                "public.ST_SRID(public.ST_Collect(sg.geometry)) AS srid\n" +
                "FROM citydb.building b\n" +
                "INNER JOIN citydb.cityobject_genericattrib cga ON b.id = cga.cityobject_id\n" +
                "INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.parent_id\n" +
                "WHERE cga.attrname = 'uuid' AND sg.geometry IS NOT NULL\n" +
                "GROUP BY cga.strval)\n" +
                "SELECT urival, srid, \n" +
                "CASE WHEN public.ST_IsValid(geo) THEN geo ELSE ST_MakeValid(geo) END AS geometry\n" +
                "FROM rawgeo;";

        String createIndex = "CREATE INDEX %s ON %s USING gist (geometry)";

        String tableFormat = "public.\"citydb_%s\"";
        String citydbPoint = String.format(tableFormat, UUID.randomUUID());
        String citydbPolygon;

        try (Connection conn = rdbStoreClient.getConnection()) {
            Statement statement = conn.createStatement();

            statement.execute(String.format(citydbTable, citydbPoint, pointSRID));
            statement.execute(String.format(createIndex, "geo_id", citydbPoint));
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }

        if (pointSRID == polygonSRID) {
            citydbPolygon = citydbPoint;
        }
        else {
            citydbPolygon = String.format(tableFormat, UUID.randomUUID());

            try (Connection conn = rdbStoreClient.getConnection()) {
                Statement statement = conn.createStatement();

                statement.execute(String.format(citydbTable, citydbPolygon, polygonSRID));
                statement.execute(String.format(createIndex, "geom_id", citydbPolygon));
            }
            catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }

        return Arrays.asList(citydbPoint, citydbPolygon);
    }

    /**
     * Delete the temporary tables
     * @param rdbStoreClient RemoteRDBStoreClient to where the OSM and CityDB data are stored
     * @param citydbPoint temporary point table name
     * @param citydbPolygon temporary polygon table name
     */
    private static void deleteTable(RemoteRDBStoreClient rdbStoreClient, String citydbPoint, String citydbPolygon) {
        String delete = "DROP TABLE %s";

        try (Connection conn = rdbStoreClient.getConnection()) {
            Statement statement = conn.createStatement();

            statement.execute(String.format(delete, citydbPoint));
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }

        if (!citydbPoint.equals(citydbPolygon)) {
            try (Connection conn = rdbStoreClient.getConnection()) {
                Statement statement = conn.createStatement();

                statement.execute(String.format(delete, citydbPolygon));
            }
            catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Returns SQL query for the SRID of a table
     * @param table table name
     * @return SQL query for the SRID of table
     */
    private static String sridQuery(String table) {
        return "SELECT DISTINCT public.ST_SRID(p.\"geometryProperty\") AS srid FROM " + table + " p";
    }

    /**
     * Returns SQL query for the min and max ID of a table
     * @param table table name
     * @return SQL query for the min and max ID of table
     */
    private static String idQuery(String table) {
        return "SELECT MIN(ogc_fid) as min, MAX(ogc_fid) as max FROM " + table;
    }

    /**
     * SQL query to match OSM point geometries with not null building tag to CityDB building footprints, and assign the building IRI accordingly
     * @param table OSM points table
     * @param cityTable table storing CityDB building IRI and footprints, transformed to same SRID as table
     * @return SQL query
     */
    private static String pointSQL(String table, String cityTable, int id) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building_iri IS NULL AND\n" +
                "ogc_fid = %s AND\n" +
                "public.ST_Intersects(p.\"geometryProperty\", g.geometry)";

        return String.format(update, table, cityTable, id);
    }

    /**
     * SQL query to match OSM polygon geometries with not null building tag to CityDB building footprints, and assign the building IRI accordingly
     * @param table OSM polygons table
     * @param cityTable table storing CityDB building IRI and footprints, transformed to same SRID as table
     * @return SQL query
     */
    private static String polygonSQL(String table, String cityTable, double threshold, int id) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building_iri IS NULL AND landuse IS NULL AND \n" +
                "ogc_fid = %d AND\n" +
                "public.ST_Area(public.ST_Intersection(p.\"geometryProperty\", g.geometry))" +
                ">= %f*LEAST(public.ST_Area(p.\"geometryProperty\"), public.ST_Area(g.geometry))";

        return String.format(update, table, cityTable, id, threshold);
    }
}
