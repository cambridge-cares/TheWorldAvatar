package uk.ac.cam.cares.jps.agent.osmagent.geometry.object;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.util.HashMap;
import java.util.Map;
import org.json.JSONArray;

public class GeoObject {
    private String urival;
    private String geometry;
    private Integer srid;

    public void setUrival(String uri) {
        this.urival = uri;
    }

    public void setGeometry(String geom) {
        this.geometry = geom;
    }

    public void setSrid(Integer srs) {
        this.srid = srs;
    }

    public String getUrival() {
        return this.urival;
    }

    public String getGeometry() {
        return this.geometry;
    }

    public Integer getSrid() {
        return this.srid;
    }

    /**
     * Queries for and returns a map of GeoObjects storing building IRI, geometry and SRID of geometry
     * @param url database url
     * @param user database username
     * @param password database password
     * @return map with building IRI as key and GeoObject as value
     */
    public static Map<String, GeoObject> getGeoObjects(String url, String user, String password) {
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(url, user, password);

        String query = getQuery();

        JSONArray resultArray = postgisClient.executeQuery(query);

        Map<String, GeoObject> result = new HashMap<>();

        for (int i = 0; i < resultArray.length(); i++) {
            GeoObject geoObject = new GeoObject();
            geoObject.setUrival(resultArray.getJSONObject(i).getString("urival"));
            geoObject.setGeometry(resultArray.getJSONObject(i).getString("geostring"));
            geoObject.setSrid(resultArray.getJSONObject(i).getInt("srid"));
            result.put(geoObject.getUrival(), geoObject);
        }

        return result;
    }

    /**
     * Returns SQL query string for building IRI, geometry and SRID of geometry
     * @return SQL query string
     */
    public static String getQuery() {
        return "SELECT cga.urival AS urival, sg.geometry as geometry, ST_AsText(sg.geometry) AS geostring, ST_SRID(sg.geometry) AS srid " +
                "FROM citydb.building b " +
                "INNER JOIN citydb.cityobject_genericattrib cga ON b.id = cga.cityobject_id " +
                "INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.id " +
                "WHERE cga.attrname = 'iri' AND sg.geometry IS NOT NULL";
    }
}
