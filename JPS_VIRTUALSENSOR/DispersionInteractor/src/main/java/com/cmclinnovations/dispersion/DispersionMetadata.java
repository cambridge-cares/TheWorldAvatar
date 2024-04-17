package com.cmclinnovations.dispersion;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.Point;
import org.locationtech.jts.geom.Polygon;

public class DispersionMetadata {
    private String label;
    private String derivationIri;
    private Map<String, String> pollutantIriToLabelMap;
    private Map<String, String> pollutantIriToDispXYZMap;
    private Polygon scopePolygon;
    private Map<Integer, String> zMap;

    private List<Long> timesteps;

    public DispersionMetadata(String derivationIri) {
        this.derivationIri = derivationIri;
        pollutantIriToLabelMap = new HashMap<>();
        pollutantIriToDispXYZMap = new HashMap<>();
        zMap = new HashMap<>();
    }

    public void setScopeLabel(String label) {
        this.label = label;
    }

    public void setTimesteps(List<Long> timesteps) {
        if (!timesteps.isEmpty()) {
            this.timesteps = timesteps;
        }
    }

    public List<Long> getTimesteps() {
        return timesteps;
    }

    public String getLabel() {
        return label;
    }

    public String getDerivationIri() {
        return derivationIri;
    }

    public void setPollutantLabelAndDispXYZ(String pollutant, String pollutantLabel, String dispXYZ) {
        pollutantIriToLabelMap.put(pollutant, pollutantLabel);
        pollutantIriToDispXYZMap.put(pollutant, dispXYZ);
    }

    public void removePollutant(String pollutant) {
        pollutantIriToDispXYZMap.remove(pollutant);
        pollutantIriToLabelMap.remove(pollutant);
    }

    public List<String> getPollutants() {
        return new ArrayList<>(pollutantIriToDispXYZMap.keySet());
    }

    public String getDispersionXYZ(String pollutant) {
        return pollutantIriToDispXYZMap.get(pollutant);
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("time", timesteps);
        json.put("pollutants", pollutantIriToLabelMap);
        json.put("derivationIri", derivationIri);
        json.put("z", zMap);

        Point centroid = scopePolygon.getCentroid();
        JSONArray centroidJson = new JSONArray();
        centroidJson.put(centroid.getX());
        centroidJson.put(centroid.getY());

        json.put("centroid", centroidJson);

        return json;
    }

    public void setScopePolygon(Polygon scopePolygon) {
        this.scopePolygon = scopePolygon;
    }

    public void addZValue(int zValue, String zIri) {
        zMap.put(zValue, zIri);
    }
}
