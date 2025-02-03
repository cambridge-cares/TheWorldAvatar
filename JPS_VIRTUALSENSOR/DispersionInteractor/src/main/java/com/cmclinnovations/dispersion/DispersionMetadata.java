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
    private Map<String, DispOut> dispOuts;

    private class DispOut {
        private String pollutant;
        private int z;
        private int srid;
        private List<Long> time;
        private List<String> output;

        private DispOut(String pollutant, int z, int srid) {
            this.pollutant = pollutant;
            this.z = z;
            this.srid = srid;
        }
    }

    public DispersionMetadata(String derivationIri) {
        this.derivationIri = derivationIri;
        dispOuts = new HashMap<>();
    }

    public void addDispOut(String iri, String pollutant, int z, int srid) {
        DispOut dispOut = new DispOut(pollutant, z, srid);
        dispOuts.put(iri, dispOut);
    }

    public List<String> getDispOutsIRI() {
        return new ArrayList<>(dispOuts.keySet());
    }

    public void updateDispOut(String dispersionOut, List<Long> simulationTimes, List<String> dispersionOutfiles) {
        dispOuts.get(dispersionOut).time = simulationTimes;
        dispOuts.get(dispersionOut).output = dispersionOutfiles;
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
        json.put("scopePolygon", scopePolygon);
        Point centroid = scopePolygon.getCentroid();
        JSONArray centroidJson = new JSONArray();
        centroidJson.put(centroid.getX());
        centroidJson.put(centroid.getY());
        json.put("lastUpdateTime", lastUpdateTime);

        json.put("centroid", centroidJson);

        JSONArray outJson = new JSONArray();

        for (DispOut dispOut : dispOuts.values()) {
            if (dispOut.output.get(0) != null) {
                JSONObject jOut = new JSONObject();
                jOut.put("pollutant", dispOut.pollutant);
                jOut.put("height", dispOut.z);
                jOut.put("SRID", dispOut.srid);
                jOut.put("time", dispOut.time);
                jOut.put("output", dispOut.output);
                outJson.put(jOut);
            }
        }

        json.put("outputs", outJson);

        return json;
    }

    public void setScopePolygon(Polygon scopePolygon) {
        this.scopePolygon = scopePolygon;
    }

}
