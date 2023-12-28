package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.apache.commons.lang.StringUtils;
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
     * @param uriString building IRI
     * @param endpoint endpoint to building geometry data
     * @param table PostGIS table name
     * @return terrain data as byte[]
     */
    public byte[] getTerrain(String uriString, String endpoint, List<CEAGeometryData> surroundings, String table) {
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

        // query for the coordinate reference system used by the terrain data
        String sridQuery = String.format("SELECT ST_SRID(rast) as srid FROM %s LIMIT 1", table);

        Double bufferDistance;

        String crs;

        Envelope envelope = new Envelope();

        try {
            if (!surroundings.isEmpty()) {
                for (CEAGeometryData ceaGeometryData : surroundings) {
                    for (Geometry geometry : ceaGeometryData.getFootprint()) {
                        envelope.expandToInclude(geometry.getEnvelopeInternal());
                    }
                }

                crs = surroundings.get(0).getCrs();

                bufferDistance = 30.0;
            }
            else {
                CEAGeometryData ceaGeometryData = GeometryQueryHelper.getBuildingGeometry(uriString, endpoint, true);

                for (Geometry geometry : ceaGeometryData.getFootprint()) {
                    envelope.expandToInclude(geometry.getEnvelopeInternal());
                }

                crs = ceaGeometryData.getCrs();

                bufferDistance = 160.0;
            }

            crs = StringUtils.isNumeric(crs) ? "EPSG:" + crs : crs;

            List<byte[]> result = new ArrayList<>();


                JSONArray sridResult = postgisClient.executeQuery(sridQuery);

                if (sridResult.isEmpty()) {
                    return null;
                }

                Integer postgisCRS = sridResult.getJSONObject(0).getInt("srid");

                // query for terrain data
                String terrainQuery = getTerrainQuery(bufferDistance, envelope.toString(), Integer.valueOf(crs), postgisCRS, table);

                try (Connection conn = postgisClient.getConnection()) {
                    Statement stmt = conn.createStatement();
                    ResultSet terrainResult = stmt.executeQuery(terrainQuery);
                    while(terrainResult.next()) {
                        byte[] rasterBytes = terrainResult.getBytes("data");
                        result.add(rasterBytes);
                    }
                }

                if (result.size() == 1) {
                    return result.get(0);
                }
                else{
                    return null;
                }
        }
        catch (Exception e) {
            System.out.println("No terrain data retrieved, agent will run CEA with CEA's default terrain.");
            return null;
        }
    }

    /**
     * Creates a SQL query string for raster data within a square bounding box which envelope buffered by bufferDistance
     * @param bufferDistance buffer distance of envelope
     * @param envelope envelope that form as the base of square bounding box for terrain query
     * @param originalCRS CRS of envelope
     * @param postgisCRS coordinate reference system of the raster data queried
     * @param table table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(Double bufferDistance, String envelope, Integer originalCRS, Integer postgisCRS, String table) {
        // SQL commands for creating a square bounding box
        String terrainBoundary = String.format("ST_Buffer(ST_Transform((ST_GeomFromText(%s, %f), %f), %f)", envelope, originalCRS, postgisCRS, bufferDistance);

        // query result to be converted to TIF format
        String query = String.format("SELECT ST_AsTIFF(ST_Union(ST_Clip(rast, %s))) as data ", terrainBoundary);
        query = query + String.format("FROM %s ", table);
        query = query + String.format("WHERE ST_Intersects(rast, %s);", terrainBoundary);

        return query;
    }
}
