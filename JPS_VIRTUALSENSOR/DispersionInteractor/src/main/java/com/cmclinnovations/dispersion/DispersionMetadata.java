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
    private String derivationIri;
    private Polygon scopePolygon;
    private Map<String, DispXYZ> dispXYZs;

    private class DispXYZ {
        private String pollutant;
        private int z;
        private List<Long> time;
        private List<String> output;

        private DispXYZ(String pollutant, int z) {
            this.pollutant = pollutant;
            this.z = z;
        }
    }

    public DispersionMetadata(String derivationIri) {
        this.derivationIri = derivationIri;
        dispXYZs = new HashMap<>();
    }

    public void addDispXYZ(String iri, String pollutant, int z) {
        DispXYZ dispXYZ = new DispXYZ(pollutant, z);
        dispXYZs.put(iri, dispXYZ);
    }

    public List<String> getDispXYZsIRI() {
        return new ArrayList<>(dispXYZs.keySet());
    }

    public void updateDispXYZ(String dispersionXYZ, List<Long> simulationTimes, List<String> dispersionXYZfiles) {
        dispXYZs.get(dispersionXYZ).time = simulationTimes;
        dispXYZs.get(dispersionXYZ).output = dispersionXYZfiles;
    }

    public void setScopeLabel(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public String getDerivationIri() {
        return derivationIri;
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("derivationIri", derivationIri);

        Point centroid = scopePolygon.getCentroid();
        JSONArray centroidJson = new JSONArray();
        centroidJson.put(centroid.getX());
        centroidJson.put(centroid.getY());

        json.put("centroid", centroidJson);

        JSONArray xyzJson = new JSONArray();

        for (DispXYZ dispXYZ : dispXYZs.values()) {
            if (dispXYZ.output.get(0) != null) {
                JSONObject jXYZ = new JSONObject();
                jXYZ.put("pollutant", dispXYZ.pollutant);
                jXYZ.put("height", dispXYZ.z);
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
