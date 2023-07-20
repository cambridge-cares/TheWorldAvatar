package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.apache.commons.lang.StringUtils;
import org.json.JSONArray;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Envelope;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

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
     * Gets terrain data for city object
     * @param uriString city object id
     * @param route route to city object geometry data
     * @param crs coordinate reference system used by route
     * @param surroundingCoordinates  coordinates that formed the bounding box for surrounding query
     * @param table PostGIS table name
     * @return terrain data as byte[]
     */
    public byte[] getTerrain(String uriString, String route, String crs, List<Coordinate> surroundingCoordinates, String table, OntologyURIHelper uriHelper) {
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(dbUrl, dbUser, dbPassword);

        // query for the coordinate reference system used by the terrain data
        String sridQuery = String.format("SELECT ST_SRID(rast) as srid FROM public.%s LIMIT 1", table);

        Coordinate centerCoordinate;

        Double radius;

        if (!surroundingCoordinates.isEmpty()) {
            Envelope envelope = new Envelope();

            for (Coordinate coordinate : surroundingCoordinates) {
                envelope.expandToInclude(coordinate);
            }
            centerCoordinate = envelope.centre();

            Double w = envelope.getWidth();
            Double h = envelope.getHeight();

            radius = w > h ? w/2 : h/2;

            radius += 30;
        }
        else {
            String envelopeCoordinates = new GeometryQueryHelper(uriHelper).getValue(uriString, "envelope", route);

            Polygon envelopePolygon = (Polygon) GeometryHandler.toPolygon(envelopeCoordinates);

            Point center = envelopePolygon.getCentroid();

            centerCoordinate = center.getCoordinate();

            radius = 160.0;
        }

        crs = StringUtils.isNumeric(crs) ? "EPSG:" + crs : crs;

        List<byte[]> result = new ArrayList<>();

        try {
            JSONArray sridResult = postgisClient.executeQuery(sridQuery);

            if (sridResult.isEmpty()) {return null;}
            Integer postgisCRS = sridResult.getJSONObject(0).getInt("srid");

            Coordinate coordinate = GeometryHandler.transformCoordinate(centerCoordinate, crs, "EPSG:" + postgisCRS);

            // query for terrain data
            String terrainQuery = getTerrainQuery(coordinate.getX(), coordinate.getY(), radius, postgisCRS, table);

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
            return null;
        }
    }

    /**
     * Creates a SQL query string for raster data within a square bounding box of length 2*radius, center point at (x, y)
     * @param x first coordinate of center point
     * @param y second coordinate of center point
     * @param radius length of the square bounding box divided by 2
     * @param postgisCRS coordinate reference system of the raster data queried
     * @param table table storing raster data
     * @return SQL query string
     */
    private String getTerrainQuery(Double x, Double y, Double radius, Integer postgisCRS, String table) {
        // SQL commands for creating a square bounding box
        String terrainBoundary = String.format("ST_Expand(ST_SetSRID(ST_MakePoint(%f, %f), %d), %f)", x, y, postgisCRS, radius);

        // query result to be converted to TIF format
        String query = String.format("SELECT ST_AsTIFF(ST_Union(ST_Clip(rast, %s))) as data ", terrainBoundary);
        query = query + String.format("FROM public.%s ", table);
        query = query + String.format("WHERE ST_Intersects(rast, %s);", terrainBoundary);

        return query;
    }
}
