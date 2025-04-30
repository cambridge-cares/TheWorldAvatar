package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;
import org.json.JSONArray;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

public class TerrainHelper {
    String dbUrl;
    String dbUser;
    String dbPassword;

    public TerrainHelper(String url, String user, String password) {
        this.dbUrl = url;
        this.dbUser = user;
        this.dbPassword = password;
    }

    /**
     * Gets terrain data for building
     * 
     * @param buildings    ArrayList of CEABuildingData of target buildings
     * @param surroundings list of CEA
     * @param table        PostGIS table name
     * @return terrain data as byte[]
     */
    public byte[] getTerrain(ArrayList<CEABuildingData> buildings, List<CEAGeometryData> surroundings, String table) {
        // query for the coordinate reference system used by the terrain data
        String sridQuery = String.format("SELECT ST_SRID(rast) as srid FROM %s LIMIT 1", table);

        String crs;

        Double bufferDistance;

        try {
            RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

            JSONArray sridResult = postgisClient.executeQuery(sridQuery);

            if (sridResult.isEmpty()) {
                System.out.println("No terrain data retrieved, agent will run CEA with CEA's default terrain.");
                return null;
            }

            Integer postgisCRS = sridResult.getJSONObject(0).getInt("srid");

            List<Geometry> geometries = new ArrayList<>();

            // building data should always be included
            for (CEABuildingData building : buildings) {
                for (Geometry geometry : building.getGeometry().getFootprint()) {
                    geometries.add(geometry);
                }
            }

            crs = buildings.get(0).getGeometry().getCrs(); // this assumes all building data (and surrounding) has the
                                                           // same CRS

            if (!surroundings.isEmpty()) {

                // include surrounding building data in the bounding box
                for (CEAGeometryData ceaGeometryData : surroundings) {
                    for (Geometry geometry : ceaGeometryData.getFootprint()) {
                        geometries.add(geometry);
                    }
                }
                bufferDistance = 30.0; // 30 metre buffer

            } else {
                // use bigger buffer if there is no surrounding building
                bufferDistance = 160.0; // 160 metre buffer
            }

            String trueBoundingBox = GeometryHandler.getBufferEnvelope(geometries, crs, 0.);
            String bufferBoundingBox = GeometryHandler.getBufferEnvelope(geometries, crs, bufferDistance);

            List<byte[]> result = new ArrayList<>();

            // query for terrain data
            String terrainQuery = getTerrainQuery(trueBoundingBox, bufferBoundingBox, Integer.valueOf(crs), postgisCRS,
                    table);

            try (Connection conn = postgisClient.getConnection();
                    Statement stmt = conn.createStatement();) {

                ResultSet terrainResult = stmt.executeQuery(terrainQuery);
                while (terrainResult.next()) {
                    byte[] rasterBytes = terrainResult.getBytes("data");
                    result.add(rasterBytes);
                }
            }

            if (result.size() == 1) {
                return result.get(0);
            } else {
                return null;
            }
        } catch (Exception e) {
            System.out.println("No terrain data retrieved, agent will run CEA with CEA's default terrain.");
            return null;
        }
    }

    /**
     * Creates a SQL query string for raster data within a bounding box
     * 
     * @param checkWkt    wkt literal of the bounding box used for checking
     * @param clipWkt     wkt literal of the bounding box used for clipping
     * @param originalCRS coordinate reference system of center point
     * @param postgisCRS  coordinate reference system of the raster data queried
     * @param table       table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(String checkWkt, String clipWkt, Integer originalCRS, Integer postgisCRS,
            String table) {
        // Create bounding box from wkt, then convert to postgis CRS
        String query = String.format(
                "WITH polygon_cte AS (SELECT ST_Transform(ST_GeomFromText('%s', %d), %d) AS geom),%n", clipWkt,
                originalCRS, postgisCRS);

        // bounding box for checking that raster data covers the essential area
        // this should be completely contained by the clipping bounding box
        query = query + String.format("check_cte AS (SELECT ST_Transform(ST_GeomFromText('%s', %d), %d) AS geom),%n",
                checkWkt, originalCRS, postgisCRS);

        // clip raster data and get its bounding box as a polygon
        query = query + String.format("clipped_bbox_cte AS (\n" + //
                "SELECT ST_Envelope(ST_Union(raster_clip.clipped)) AS bounding_box,\n" + //
                "ST_AsTIFF(ST_Union(raster_clip.clipped)) AS data\n" + //
                "FROM ( SELECT ST_Clip(a.rast, b.geom) AS clipped\n" + //
                "FROM %s a, polygon_cte b WHERE ST_Intersects(a.rast, b.geom)) raster_clip)\n", table);

        // return clipped raster data if it completely covers checkWkt, otherwise this will be blank
        query = query + "SELECT clipped_bbox_cte.data FROM polygon_cte, clipped_bbox_cte, check_cte\n" +
                "WHERE ST_Contains(clipped_bbox_cte.bounding_box, check_cte.geom);";

        return query;
    }

}