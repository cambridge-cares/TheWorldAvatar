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

public class GeoMatcher {
    private static final double threshold = 0.7;

    public static void matchGeometry(String url, String user, String password, String pointTable, String polygonTable) {
        RemoteRDBStoreClient rdbStoreClient = new RemoteRDBStoreClient(url, user, password);

        int pointSRID;
        int polygonSRID;

        int partitions = 10;
        int pointMin;
        int polygonMin;
        int pointMax;
        int polygonMax;

        JSONArray result = rdbStoreClient.executeQuery(sridQuery(pointTable));

        if (!result.isEmpty()) {
            pointSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        result = rdbStoreClient.executeQuery(sridQuery(polygonTable));

        if (!result.isEmpty()) {
            polygonSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        List<String> tables = createTable(rdbStoreClient, pointSRID, polygonSRID);

        String citydbPoint = tables.get(0);
        String citydbPolygon = tables.get(1);

        result = rdbStoreClient.executeQuery(idQuery(pointTable));

        if (!result.isEmpty()) {
            pointMin = result.getJSONObject(0).getInt("min");
            pointMax = result.getJSONObject(0).getInt("max");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        result = rdbStoreClient.executeQuery(idQuery(polygonTable));

        if (!result.isEmpty()) {
            polygonMin = result.getJSONObject(0).getInt("min");
            polygonMax = result.getJSONObject(0).getInt("max");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        int pointStep = (pointMax - pointMin) / partitions;
        int polygonStep = (polygonMax - polygonMin) / partitions;

        for (int i = pointMin; i <= pointMax; i+= pointStep+1) {
            rdbStoreClient.executeUpdate(pointSQL(pointTable, citydbPoint, i, i+pointStep));
        }

        for (int i = polygonMin; i <= polygonMax; i+= polygonStep+1) {

            rdbStoreClient.executeUpdate(polygonSQL(polygonTable, citydbPolygon, threshold, i, i+polygonStep));
        }

        for (int i = pointMin; i <= pointMax; i+= pointStep+1) {
            rdbStoreClient.executeUpdate(pointSQL2(pointTable, citydbPoint, i, i+pointStep));
        }

        for (int i = polygonMin; i <= polygonMax; i+= polygonStep+1) {
            rdbStoreClient.executeUpdate(polygonSQL2(polygonTable, citydbPolygon, threshold, i, i+polygonStep));
        }

        deleteTable(rdbStoreClient, citydbPoint, citydbPolygon);

        // match OSM building point geometry with cityDB building IRI
        // match OSM building polygon geometry with cityDB building IRI
        // match OSM non-building point geometry with cityDB building IRI
        // match OSM non-building and non-landplot polygon geometry with cityDB building IRI
    }


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

        String createIndex = "CREATE INDEX geo_id ON %s USING gist (geometry)";

        String tableFormat = "public.\"citydb_%s\"";
        String citydbPoint = String.format(tableFormat, UUID.randomUUID());
        String citydbPolygon;

        try (Connection conn = rdbStoreClient.getConnection()) {
            Statement statement = conn.createStatement();

            statement.execute(String.format(citydbTable, citydbPoint, pointSRID));
            statement.execute(String.format(createIndex, citydbPoint));
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
                statement.execute(String.format(createIndex, citydbPolygon));
            }
            catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }

        return Arrays.asList(citydbPoint, citydbPolygon);
    }

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

    private static String sridQuery(String table) {
        return "SELECT DISTINCT public.ST_SRID(p.\"geometryProperty\") AS srid FROM " + table + " p";
    }

    private static String idQuery(String table) {
        return "SELECT MIN(ogc_fid) as min, MAX(ogc_fid) as max FROM " + table;
    }

    private static String pointSQL(String table, String cityTable, int start, int end) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building IS NOT NULL AND building_iri IS NULL AND\n" +
                "ogc_fid BETWEEN %d AND %d AND\n" +
                "public.ST_Intersects(p.\"geometryProperty\", g.geometry)";

        return String.format(update, table, cityTable, start, end);
    }

    private static String pointSQL2(String table, String cityTable, int start, int end) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building IS NULL AND building_iri IS NULL AND\n" +
                "ogc_fid BETWEEN %d AND %d AND\n" +
                "public.ST_Intersects(p.\"geometryProperty\", g.geometry)";

        return String.format(update, table, cityTable, start, end);
    }

    private static String polygonSQL(String table, String cityTable, Double threshold, int start, int end) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building IS NOT NULL AND building_iri IS NULL AND landuse IS NULL AND \n" +
                "ogc_fid BETWEEN %d AND %d AND\n" +
                "public.ST_Area(public.ST_Intersection(p.\"geometryProperty\", g.geometry))" +
                ">= %f*public.ST_Area(p.\"geometryProperty\")";

        return String.format(update, table, cityTable, start, end, threshold);
    }

    private static String polygonSQL2(String table, String cityTable, Double threshold, int start, int end) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building IS NULL AND building_iri IS NULL AND landuse IS NULL AND \n" +
                "ogc_fid BETWEEN %d AND %d AND\n" +
                "public.ST_Area(public.ST_Intersection(p.\"geometryProperty\", g.geometry))" +
                ">= %f*public.ST_Area(p.\"geometryProperty\")";

        return String.format(update, table, cityTable, start, end, threshold);
    }
}
