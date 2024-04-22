package com.cmclinnovations.dispersion;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

public class DispersionMetadata {
    private String label;
    private String scopeIri;
    private String derivationIri;
    private Polygon scopePolygon;
    private int lastUpdateTime;
    private Map<String, DispXYZ> dispXYZs;

    private class DispXYZ {
        private String pollutant;
        private int z;
        private int srid;
        private List<Long> time;
        private List<String> output;

        private DispXYZ(String pollutant, int z, int srid) {
            this.pollutant = pollutant;
            this.z = z;
            this.srid = srid;
        }
    }

    public DispersionMetadata(String derivationIri) {
        this.derivationIri = derivationIri;
        dispXYZs = new HashMap<>();
    }

    public void addDispXYZ(String iri, String pollutant, int z, int srid) {
        DispXYZ dispXYZ = new DispXYZ(pollutant, z, srid);
        dispXYZs.put(iri, dispXYZ);
    }

    public List<String> getDispXYZsIRI() {
        return new ArrayList<>(dispXYZs.keySet());
    }

    public void updateDispXYZ(String dispersionXYZ, List<Long> simulationTimes, List<String> dispersionXYZfiles) {
        dispXYZs.get(dispersionXYZ).time = simulationTimes;
        dispXYZs.get(dispersionXYZ).output = dispersionXYZfiles;
    }

    public void setScope(String scopeIri, String label) {
        this.scopeIri = scopeIri;
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public String getScopeIri() {
        return scopeIri;
    }

    public String getDerivationIri() {
        return derivationIri;
    }

    public void setLastUpdateTime(int lastUpdateTime) {
        this.lastUpdateTime = lastUpdateTime;
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("derivationIri", derivationIri);
        json.put("scopeIri", scopeIri);
        Point centroid = scopePolygon.getCentroid();
        JSONArray centroidJson = new JSONArray();
        centroidJson.put(centroid.getX());
        centroidJson.put(centroid.getY());
        json.put("lastUpdateTime", lastUpdateTime);

        json.put("centroid", centroidJson);

        JSONArray xyzJson = new JSONArray();

        for (DispXYZ dispXYZ : dispXYZs.values()) {
            if (dispXYZ.output.get(0) != null) {
                JSONObject jXYZ = new JSONObject();
                jXYZ.put("pollutant", dispXYZ.pollutant);
                jXYZ.put("height", dispXYZ.z);
                jXYZ.put("SRID", dispXYZ.srid);
                jXYZ.put("time", dispXYZ.time);
                jXYZ.put("output", dispXYZ.output);
                xyzJson.put(jXYZ);
            }
        }

        json.put("outputs", xyzJson);

        return json;
    }

    public void setScopePolygon(Polygon scopePolygon) {
        this.scopePolygon = scopePolygon;
    }

}
