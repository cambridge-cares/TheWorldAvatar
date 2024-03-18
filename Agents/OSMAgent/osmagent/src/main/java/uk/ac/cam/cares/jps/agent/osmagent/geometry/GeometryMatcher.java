package uk.ac.cam.cares.jps.agent.osmagent.geometry;

import com.opencsv.CSVReader;
import com.opencsv.CSVReaderBuilder;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.agent.osmagent.FileReader;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

public class GeometryMatcher {
    private static final double threshold = 0.7;
    private static final String RESOURCES_PATH = "/resources";

    private final RemoteRDBStoreClient rdbStoreClient;
    public GeometryMatcher(String url, String user, String password) {
        this.rdbStoreClient  = new RemoteRDBStoreClient(url, user, password);
    }

    public void matchGeometry(String pointTable, String polygonTable, String bound, Integer boundSRID) {

        int pointSRID;
        int polygonSRID;

        int pointMin;
        int polygonMin;
        int pointMax;
        int polygonMax;

        // get SRID of OSM point geometries
        JSONArray result = rdbStoreClient.executeQuery(sridQuery(pointTable, "geometryProperty"));

        if (!result.isEmpty()) {
            pointSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // get SRID of OSM polygon geometries
        result = rdbStoreClient.executeQuery(sridQuery(polygonTable, "geometryProperty"));

        if (!result.isEmpty()) {
            polygonSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        // create temporary tables of CityDB building footprints, transformed to the same SRIDs of OSM point and polygon geometries
        String citydbPoint = createTable(bound, boundSRID, pointSRID);

        String citydbPolygon = "";

        if (pointSRID != polygonSRID) {
            citydbPolygon = createTable(bound, boundSRID, polygonSRID);
        }

        if (bound == null) {
            // get min and max IDs for points table
            result = rdbStoreClient.executeQuery(idQuery(pointTable));

            if (!result.isEmpty()) {
                pointMin = result.getJSONObject(0).getInt("min");
                pointMax = result.getJSONObject(0).getInt("max");
            } else {
                throw new JPSRuntimeException("Failed, no ogc_fid column in OSM points table.");
            }

            // get min and max IDs for polygons table
            result = rdbStoreClient.executeQuery(idQuery(polygonTable));

            if (!result.isEmpty()) {
                polygonMin = result.getJSONObject(0).getInt("min");
                polygonMax = result.getJSONObject(0).getInt("max");
            } else {
                throw new JPSRuntimeException("Failed, no ogc_fid column in OSM points table.");
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

            System.out.println("Finished OSM points matching.");

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

            System.out.println("Finished OSM polygons matching.");
        }
        else {
            result = rdbStoreClient.executeQuery(boundID(pointTable, bound, boundSRID, pointSRID));

            try (Connection connection = rdbStoreClient.getConnection();
                 Statement statement = connection.createStatement()) {
                for (int i = 0; i < result.length(); i++) {
                    statement.execute(pointSQL(pointTable, citydbPoint, result.getJSONObject(i).getInt("ogc_fid")));
                }
            }
            catch (SQLException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }

            System.out.println("Finished OSM points matching.");

            result = rdbStoreClient.executeQuery(boundID(polygonTable, bound, boundSRID, polygonSRID));

            try (Connection connection = rdbStoreClient.getConnection();
                 Statement statement = connection.createStatement()) {
                for (int i = 0; i < result.length(); i++) {
                    statement.execute(polygonSQL(polygonTable, citydbPoint, threshold, result.getJSONObject(i).getInt("ogc_fid")));
                }
            }
            catch (SQLException e) {
                e.printStackTrace();
                throw new JPSRuntimeException(e);
            }

            System.out.println("Finished OSM polygons matching.");
        }

        // delete temporary tables
        deleteTable(citydbPoint);
        if (!citydbPolygon.isEmpty()) {
            deleteTable(citydbPolygon);
        }
    }


    private String createTable(String bound, Integer boundSRID, Integer targetSRID) {
        String citydbTable = "CREATE TABLE %s AS \n" +
                "WITH rawgeo AS (\n" +
                "SELECT cga.strval AS urival, public.ST_Transform(public.ST_Collect(sg.geometry), %d) as geo \n" +
                "FROM citydb.building b\n" +
                "INNER JOIN citydb.cityobject_genericattrib cga ON b.id = cga.cityobject_id\n" +
                "INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.parent_id\n" +
                "WHERE cga.attrname = 'uuid' AND sg.geometry IS NOT NULL\n" +
                "GROUP BY cga.strval)%s\n" +
                "SELECT urival, \n" +
                "CASE WHEN public.ST_IsValid(geo) THEN geo ELSE ST_MakeValid(geo) END AS geometry\n" +
                "FROM rawgeo%s;";

        String boundGeo = "";
        String intersects = "";

        if (bound != null) {
            boundGeo = ",\n" +
                    "bound AS (" +
                    "SELECT %s AS geo)";
            boundGeo = String.format(boundGeo, boundGeometry(bound, boundSRID, targetSRID));
            intersects =  " WHERE public.ST_Intersects((SELECT geo FROM bound LIMIT 1), geo)";
        }

        String createIndex = "CREATE INDEX %s ON %s USING gist (geometry)";

        String tableFormat = "public.\"citydb_%s\"";
        String cityTable = String.format(tableFormat, UUID.randomUUID());

        try (Connection conn = rdbStoreClient.getConnection()) {
            Statement statement = conn.createStatement();

            statement.execute(String.format(citydbTable, cityTable, targetSRID, boundGeo, intersects));
            statement.execute(String.format(createIndex, String.format("\"geo_id_%s\"", UUID.randomUUID()), cityTable));
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return cityTable;
    }

    /**
     * Delete the temporary tables
     */
    private void deleteTable(String table) {
        String delete = "DROP TABLE %s";

        try (Connection conn = rdbStoreClient.getConnection()) {
            Statement statement = conn.createStatement();

            statement.execute(String.format(delete, table));
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Returns SQL query for the SRID of a table
     * @param table table name
     * @return SQL query for the SRID of table
     */
    private String sridQuery(String table, String geometryColumn) {
        return String.format("SELECT DISTINCT public.ST_SRID(p.\"%s\") AS srid FROM %s p", geometryColumn, table);
    }

    /**
     * Returns SQL query for the min and max ID of a table
     * @param table table name
     * @return SQL query for the min and max ID of table
     */
    private String idQuery(String table) {
        return "SELECT MIN(ogc_fid) as min, MAX(ogc_fid) as max FROM " + table;
    }

    private String boundGeometry(String bound, Integer boundSRID, Integer targetSRID) {
        return String.format("public.ST_Transform(public.ST_GeomFromText(\'%s\', %d), %d)", bound, boundSRID, targetSRID);
    }

    private String boundID(String table, String bound, Integer boundSRID, Integer targetSRID) {
        String with = "WITH bound AS (SELECT %s AS geo)\n";
        with = String.format(with, boundGeometry(bound, boundSRID, targetSRID));

        return with + String.format("SELECT ogc_fid FROM %s WHERE public.ST_Intersects((SELECT geo FROM bound LIMIT 1), \"geometryProperty\")", table);
    }

    /**
     * SQL query to match OSM point geometries with not null building tag to CityDB building footprints, and assign the building IRI accordingly
     * @param table OSM points table
     * @param cityTable table storing CityDB building IRI and footprints, transformed to same SRID as table
     * @return SQL query
     */
    private String pointSQL(String table, String cityTable, int id) {
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
    private String polygonSQL(String table, String cityTable, double threshold, int id) {
        String update = "UPDATE %s AS p\n" +
                "SET building_iri = g.urival\n" +
                "FROM %s AS g\n" +
                "WHERE building_iri IS NULL AND landuse IS NULL AND \n" +
                "ogc_fid = %d AND\n" +
                "public.ST_Area(public.ST_Intersection(p.\"geometryProperty\", g.geometry))" +
                ">= %f*LEAST(public.ST_Area(p.\"geometryProperty\"), public.ST_Area(g.geometry))";

        return String.format(update, table, cityTable, id, threshold);
    }

    /**
     * Matches building IRIs not in usageTable with land use from landUseTable, and updates usageTable with the assigned OntoBuiltEnv:PropertyUsage class according to '/dlm_landuse.csv'
     * @param usageTable centralised table to store usage information
     * @param landUseTable table containing DLM land use data
     */
    public void updateLandUse(String usageTable, String landUseTable, String geometryColumn, String csv, String bound, Integer boundSRID) {
        JSONArray result = rdbStoreClient.executeQuery(sridQuery(landUseTable, geometryColumn));

        int landSRID;

        if (!result.isEmpty()) {
            landSRID = result.getJSONObject(0).getInt("srid");
        }
        else {
            throw new JPSRuntimeException("fail");
        }

        String citydbTable = createTable( bound, boundSRID, landSRID);

        try (InputStream input = FileReader.getStream(RESOURCES_PATH + "/" + csv)) {
            InputStreamReader inputStreamReader = new InputStreamReader(input);
            CSVReader csvReader = new CSVReaderBuilder(inputStreamReader).withSkipLines(1).build();
            String[] line;

            while ((line = csvReader.readNext()) != null) {
                String ontobuilt = line[3];
                String key = line[0];
                String value = line[1];

                String updateLandUse = "INSERT INTO " + usageTable + " (building_iri, ontobuilt) \n" +
                        "SELECT q2.iri, \'" + ontobuilt + "\' FROM \n" +
                        "(SELECT q.urival AS iri, q.geometry AS geo FROM " + citydbTable + "q\n" +
                        "LEFT JOIN " + usageTable + " u ON q.urival = u.building_iri \n" +
                        "WHERE u.building_iri IS NULL) AS q2 \n" +
                        "WHERE public.ST_Intersects(q2.geo, \n" +
                        "(SELECT CASE WHEN public.ST_IsValid(public.ST_Collect(\"%s\")) THEN public.ST_Collect(\"%s\")\n" +
                        "ELSE public.ST_MakeValid(public.ST_Collect(\"%s\")) END AS g FROM " + landUseTable + " \n" +
                        "WHERE \"" + key + "\" = \'" + value + "\'))";

                rdbStoreClient.executeUpdate(String.format(updateLandUse, geometryColumn, geometryColumn, geometryColumn));
                System.out.println(
                        "Untagged buildings with building_iri are assigned for " + key + " with value:"
                                + value + " under the ontobuiltenv:" + ontobuilt
                                + " category.");
            }

            System.out.println(
                    "Untagged building has been assigned an ontobuilt type according to the corresponding landuse.");
            csvReader.close();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("dlm_landuse.csv file not found");
        }
        catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
        finally {
            deleteTable(citydbTable);
        }
    }
}
