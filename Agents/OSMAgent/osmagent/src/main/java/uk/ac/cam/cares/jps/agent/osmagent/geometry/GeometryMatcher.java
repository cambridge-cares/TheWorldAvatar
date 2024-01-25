package uk.ac.cam.cares.jps.agent.osmagent.geometry;

import uk.ac.cam.cares.jps.agent.osmagent.geometry.object.*;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.json.JSONArray;

public class GeometryMatcher {
    public static final String epsg4326 = "4326";
    private final double threshold = 0.7;

    private Map<String, GeoObject> geoObjects;

    private final String dbUrl;
    private final String user;
    private final String password;

    private RemoteRDBStoreClient postgisClient;

    public GeometryMatcher(String postgisDb, String postgisUser, String postgisPassword) {
        this.dbUrl = postgisDb;
        this.user = postgisUser;
        this.password = postgisPassword;
        this.geoObjects = GeoObject.getGeoObjects(dbUrl, user, password);
        this.postgisClient = new RemoteRDBStoreClient(dbUrl, user, password);
    }

    /**
     * Matches building geometry and OpenStreetMap geometry, then updates OSM table with matched building IRI
     * @param table OSM table
     */
    public void matchGeometry(String table) throws ParseException {
        WKTReader wktReader = new WKTReader();

        String condition = "building IS NOT NULL AND building_iri IS NULL";

        boolean flag = checkIfPoints(table);

        if (!flag) {
            condition = "landuse IS NULL AND " + condition;
        }

        Map<Integer, OSMObject> osmObjects = OSMObject.getOSMObjects(dbUrl, user, password, table, condition);

        String updateStart = "UPDATE " + table + " SET building_iri = CASE";
        String updateEnd = " ELSE building_iri END";

        List<String> updates = new ArrayList<>();
        int counter = 0;
        String update = "";

        System.out.println(table + " matching start");

        // match building geometry with OSM building geometry
        for (Map.Entry<Integer, OSMObject> entry : osmObjects.entrySet()) {
            OSMObject osmObject = entry.getValue();

            if (counter == 500) {
                updates.add(update);
                counter = 0;
                update = "";
            }

            if (osmObject.getGeometry().contains("POINT")) {
                String query = booleanQuery(osmObject);

                JSONArray result = postgisClient.executeQuery(query);

                if (!result.isEmpty()) {
                    osmObject.setIri(result.getJSONObject(0).getString("building_iri"));
                }
            }
            else {
                String query = iriQuery(osmObject, threshold);

                JSONArray matchResult = postgisClient.executeQuery(query);

                if (!matchResult.isEmpty()) {
                    osmObject.setIri(matchResult.getJSONObject(0).getString("building_iri"));
                }
            }

            if (!osmObject.getIri().isEmpty()) {
                geoObjects.remove(osmObject.getIri());
                update += " " + caseOgcFid(osmObject.getOgcfid(), osmObject.getIri());
                counter++;
            }
        }

        if (!update.isEmpty()) {
            updates.add(update);
        }

        // update OSM table with matched building IRI
        for (String str : updates) {
            postgisClient.executeUpdate(updateStart + str + updateEnd);
        }

        updates.clear();
        counter = 0;
        update = "";

        // match building geometry, which were not matched in the first round, with OSM geometry that don't have building IRI
        for (Map.Entry<String, GeoObject> entry : geoObjects.entrySet()) {
            GeoObject geoObject = entry.getValue();

            if (counter == 100) {
                updates.add(update);
                counter = 0;
                update = "";
            }

            String query = queryFromOSM(table, geoObject, flag, threshold);

            JSONArray result = postgisClient.executeQuery(query);

            String iriUpdate = "";

            if (!result.isEmpty()) {
                iriUpdate = caseOgcFid(result.getJSONObject(0).getInt("id"), geoObject.getUrival());
                for (int i = 1; i < result.length(); i++) {
                    iriUpdate = insertUpdate(iriUpdate, result.getJSONObject(i).getInt("id"));
                }
            }

            if (!iriUpdate.isEmpty()) {
                update += " " + iriUpdate;
                counter++;
            }
        }

        if (!update.isEmpty()) {
            updates.add(update);
        }

        // update OSM table with matched building IRI
        for (String str : updates) {
            postgisClient.executeUpdate(updateStart + str + updateEnd);
        }
        System.out.println(table + " matching end");
    }

    /**
     * Checks whether if a table has point geometry
     * @param table table to check
     * @return true if point geometry, false otherwise
     */
    private boolean checkIfPoints(String table) {
        boolean flag = false;
        String query = "SELECT ST_ASText(\"geometryProperty\") as geostring FROM " + table + " WHERE \"geometryProperty\" IS NOT NULL LIMIT 1";
        JSONArray result = postgisClient.executeQuery(query);

        if (!result.isEmpty()) {
            flag = result.getJSONObject(0).getString("geostring").contains("POINT") ? true : false;
        }

        return flag;
    }

    /**
     * Returns SQL query string from table with OpenStreetMap data
     * @param table table to query from
     * @param geoObject GeoObject
     * @param flag flag indicating whether table contains point geometry
     * @param threshold intersection area ratio threshold
     * @return SQL query string
     */
    private String queryFromOSM(String table, GeoObject geoObject, Boolean flag, Double threshold) {
        String query;
        Integer srid = geoObject.getSrid();
        String geometry = geoObject.getGeometry();

        if (flag) {
            query = "SELECT ogc_fid AS id FROM " + table + " WHERE \"geometryProperty\" IS NOT NULL AND public.ST_Intersects(" + geometryString(geometry, srid) + "," +
                    "public.ST_Transform(\"geometryProperty\"," + srid + ")) AND building_iri IS NULL";
        }
        else {
            String gmlString  = "WITH gmlgeo AS (SELECT public.ST_Transform(" + geometryString(geometry, srid)
                    + "," + epsg4326 + ") AS geo FROM citydb.building LIMIT 1) \n";
            String subQuery = "(SELECT ogc_fid, public.ST_Area(public.ST_Intersection((SELECT geo FROM gmlgeo),public.ST_Transform(\"geometryProperty\"," + epsg4326 + ")),true) AS matchedarea," +
                    "public.ST_Area(public.ST_Transform(\"geometryProperty\"," + epsg4326 +"),true) AS area FROM " + table +
                    " WHERE \"geometryProperty\" IS NOT NULL AND landuse IS NULL AND building IS NULL AND building_iri is NULL) AS q";

            query = "SELECT q.ogc_fid AS id FROM " + subQuery + " WHERE (matchedarea / area) >= " + threshold + " OR matchedarea / (SELECT public.ST_Area(geo, true) FROM gmlgeo) >= " + threshold;
            query = gmlString + query;
        }

        return query;
    }

    /**
     * Returns SQL query string for building geometry and IRI as sub query
     * @return SQL query string
     */
    private String subQuery() {
        return " (" + GeoObject.getQuery() + ") AS q";
    }

    /**
     * Returns SQL query string for whether a building geometry intersects with OpenStreetMap geometry
     * @param osmObject OSMObject
     * @return SQL query string
     */
    private String booleanQuery(OSMObject osmObject) {
        String osmObjectString = transformString(geometryString(osmObject.getGeometry(), osmObject.getSrid()));

        return "SELECT q.urival AS building_iri FROM " +  subQuery() + " WHERE public.ST_Intersects(q.geometry," + osmObjectString + ")";
    }

    /**
     * Returns building IRI intersection area between building geometry and OpenStreetMap geometry that meets the rquired threshold
     * @param osmObject OSMObject
     * @param threshold intersection area ratio threshold
     * @return SQL query string
     */
    private String iriQuery(OSMObject osmObject, Double threshold) {
        String osmObjectString = "WITH osmgeo AS (SELECT public.ST_Transform(" + geometryString(osmObject.getGeometry(), osmObject.getSrid())
                + "," + epsg4326 + ") AS geo FROM citydb.building LIMIT 1) \n";

        String query =  "SELECT public.ST_Area(public.ST_Intersection(public.ST_Transform(q.geometry," + epsg4326 + "), (SELECT geo FROM osmgeo)), true) AS matchedarea, " +
                "public.ST_Area(public.ST_Transform(q.geometry," + epsg4326 + "), true) AS gmlarea, q.urival AS building_iri";
        query += " FROM \n" + subQuery();

        return osmObjectString + "SELECT building_iri FROM (" + query + ") AS intersection \n" +
                "WHERE intersection.matchedarea / (SELECT public.ST_Area(geo, true) FROM osmgeo) >= " + threshold + " OR matchedarea / intersection.gmlarea >= " + threshold;
    }

    /**
     * Returns SQL query string for creating geometry object from WKT string
     * @param wkt WKT string
     * @param srid SRID of wkt
     * @return SQL query string
     */
    private String geometryString(String wkt, Integer srid) {
        return "public.ST_GeomFromText(\'" + wkt + "\'," + srid + ")";
    }

    /**
     * Returns SQL query string to transform a geometry to the same SRID as building geometry
     * @param geometry geometry object
     * @return SQL query string
     */
    private String transformString(String geometry) {
        return "public.ST_Transform(" + geometry + ",ST_Srid(q.geometry))";
    }

    /**
     * Inserts case conditions to SQL case statement where the building IRI is the same
     * @param query SQL case statement string
     * @param ogcFid ogc_fid of the OSMObject
     * @return SQL case statement string
     */
    private String insertUpdate(String query, Integer ogcFid) {
       String[] split = query.split("THEN");

       return split[0] + "OR ogc_fid = " + ogcFid + " THEN" + split[1];
    }

    /**
     * SQL case condition string
     * @param ogcFid ogc_fid of the condition
     * @param iri IRI to set to
     * @return SQL case condition string
     */
    private String caseOgcFid(Integer ogcFid, String iri) {
        return "WHEN ogc_fid = " + ogcFid + " THEN \'" + iri + "\'";
    }
}
