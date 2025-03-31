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

            if (!surroundings.isEmpty()) {

                // create an expanded envelope based on surrounding building data
                for (CEAGeometryData ceaGeometryData : surroundings) {
                    for (Geometry geometry : ceaGeometryData.getFootprint()) {
                        geometries.add(geometry);
                    }
                }

                crs = surroundings.get(0).getCrs();

                bufferDistance = 30.0; // 30 metre buffer

            } else {

                // create an expanded envelope based on asked building data because there is no
                // surrounding buildings
                for (CEABuildingData building : buildings) {
                    for (Geometry geometry : building.getGeometry().getFootprint()) {
                        geometries.add(geometry);
                    }
                }

                crs = buildings.get(0).getGeometry().getCrs();

                bufferDistance = 160.0; // 160 metre buffer
            }

            String boundingBox = GeometryHandler.getBufferEnvelope(geometries, crs, bufferDistance);

            List<byte[]> result = new ArrayList<>();

            // query for terrain data
            String terrainQuery = getTerrainQuery(boundingBox, Integer.valueOf(crs),
                    postgisCRS, table);

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
     * @param wkt         wkt literal of the bounding box
     * @param originalCRS coordinate reference system of center point
     * @param postgisCRS  coordinate reference system of the raster data queried
     * @param table       table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(String wkt, Integer originalCRS, Integer postgisCRS,
            String table) {
        // Create bounding box from wkt, then convert to postgis CRS
        String query = String.format(
                "WITH polygon_cte AS (SELECT ST_Transform(ST_GeomFromText('%s', %d), %d) AS geom)%n", wkt, originalCRS,
                postgisCRS);

        query = query + String.format("SELECT ST_AsTIFF(ST_Union(raster_clip.clipped)) AS data FROM (\n" + //
                "    SELECT ST_Clip(a.rast, b.geom) AS clipped\n" + //
                "    FROM %s a, polygon_cte b\n" + //
                "    WHERE ST_Intersects(a.rast, b.geom)) raster_clip;", table);

        return query;
    }

}