package uk.ac.cam.cares.jps.agent.osmagent.geometry.object;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.util.HashMap;
import java.util.Map;
import org.json.JSONArray;

public class OSMObject {
    private Integer ogcfid;
    private String geometry;
    private Integer srid;
    private String iri = "";

    public void setOgcfid(Integer id) {
        this.ogcfid = id;
    }

    public void setGeometry(String geom) {
        this.geometry = geom;
    }

    public void setSrid(Integer srs) {
        this.srid = srs;
    }

    public void setIri(String uri) {
        this.iri = uri;
    }

    public Integer getOgcfid() {
        return this.ogcfid;
    }

    public String getGeometry() {
        return this.geometry;
    }

    public Integer getSrid() {
        return this.srid;
    }

    public String getIri() {
        return this.iri;
    }

    public static Map<Integer, OSMObject> getOSMObject(String url, String user, String password, String table, String filter) {
        RemoteRDBStoreClient postgisClient = new RemoteRDBStoreClient(url, user, password);

        String query = "SELECT ST_AsText(\"geometryProperty\") AS geometry, ST_SRID(\"geometryProperty\") AS srid, ogc_fid " + "FROM " + table;

        if (!filter.isEmpty()) {
            query += " WHERE " + filter;
        }

        JSONArray resultArray = postgisClient.executeQuery(query);

        Map<Integer, OSMObject> result = new HashMap<>();

        for (int i = 0; i < resultArray.length(); i++) {
            OSMObject osmObject = new OSMObject();
            osmObject.setOgcfid(resultArray.getJSONObject(i).getInt("ogc_fid"));
            osmObject.setGeometry(resultArray.getJSONObject(i).getString("geometry"));
            osmObject.setSrid(resultArray.getJSONObject(i).getInt("srid"));
            result.put(osmObject.getOgcfid(), osmObject);
        }

        return result;
    }
}
