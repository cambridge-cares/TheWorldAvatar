package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;
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
     * @param buildings ArrayList of CEABuildingData of target buildings
     * @param surroundings list of CEA
     * @param table PostGIS table name
     * @return terrain data as byte[]
     */
    public byte[] getTerrain(ArrayList<CEABuildingData> buildings, List<CEAGeometryData> surroundings, String table) {
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

        // query for the coordinate reference system used by the terrain data
        String sridQuery = String.format("SELECT ST_SRID(rast) as srid FROM %s LIMIT 1", table);

        Double bufferDistance;

        Coordinate center;

        String crs;

        Envelope envelope = new Envelope();

        JSONArray sridResult = postgisClient.executeQuery(sridQuery);

        if (sridResult.isEmpty()) {
            return null;
        }

        Integer postgisCRS = sridResult.getJSONObject(0).getInt("srid");

        try {
            if (!surroundings.isEmpty()) {
                for (CEAGeometryData ceaGeometryData : surroundings) {
                    for (Geometry geometry : ceaGeometryData.getFootprint()) {
                        envelope.expandToInclude(geometry.getEnvelopeInternal());
                    }
                }

                crs = surroundings.get(0).getCrs();

                center = envelope.centre();

                Polygon polygon = envelopeToPolygon(envelope);

                polygon = (Polygon) GeometryHandler.transformGeometry(polygon, "EPSG:"+crs, "EPSG:"+postgisCRS);

                envelope = new Envelope();
                for (Coordinate coordinate : polygon.getCoordinates()) {
                    envelope.expandToInclude(coordinate);
                }

                double w = envelope.getWidth();
                double h = envelope.getHeight();

                bufferDistance = w >= h ? w /2 : h/2;

                bufferDistance += 30.0;
            }
            else {
                for (CEABuildingData building : buildings) {
                    for (Geometry geometry : building.getGeometry().getFootprint()) {
                        envelope.expandToInclude(geometry.getEnvelopeInternal());
                    }
                }

                crs = buildings.get(0).getGeometry().getCrs();

                center = envelope.centre();
                bufferDistance = 160.0;
            }

            List<byte[]> result = new ArrayList<>();

            // query for terrain data
            String terrainQuery = getTerrainQuery(center.getX(), center.getY(), bufferDistance, Integer.valueOf(crs), postgisCRS, table);

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
     * Creates a SQL query string for raster data within a square bounding box of length 2*radius, center point at (x, y)
     * @param x first coordinate of center point
     * @param y second coordinate of center point
     * @param radius length of the square bounding box divided by 2
     * @param originalCRS coordinate reference system of center point
     * @param postgisCRS coordinate reference system of the raster data queried
     * @param table table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(Double x, Double y, Double radius, Integer originalCRS, Integer postgisCRS, String table) {
        // SQL commands for creating a square bounding box
        String terrainBoundary = String.format("ST_Expand(ST_Transform(ST_SetSRID(ST_MakePoint(%f, %f), %d), %d), %f)", x, y, originalCRS, postgisCRS, radius);

        // query result to be converted to TIF format
        String query = String.format("SELECT ST_AsTIFF(ST_Union(ST_Clip(rast, %s))) as data ", terrainBoundary);
        query = query + String.format("FROM %s ", table);
        query = query + String.format("WHERE ST_Intersects(rast, %s);", terrainBoundary);

        return query;
    }

    /**
     * Converts an Envelope object to Polygon object
     * @param envelope Envelope object
     * @return envelope as a Polygon object
     */
    private Polygon envelopeToPolygon(Envelope envelope) {
        GeometryFactory geometryFactory = new GeometryFactory();

        Coordinate[] coordinates = new Coordinate[5];
        coordinates[0] = new Coordinate(envelope.getMinX(), envelope.getMinY());
        coordinates[1] = new Coordinate(envelope.getMinX(), envelope.getMaxY());
        coordinates[2] = new Coordinate(envelope.getMaxX(), envelope.getMaxY());
        coordinates[3] = new Coordinate(envelope.getMaxX(), envelope.getMinY());
        coordinates[4] = coordinates[0];

        return geometryFactory.createPolygon(coordinates);
    }
}
