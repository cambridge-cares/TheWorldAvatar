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

public class DispersionSimulation {
    private String label;
    private String derivationIri;
    private Map<String, String> pollutantIriToLabelMap;
    private Map<String, String> pollutantIriToDispLayerMap;
    private Polygon scopePolygon;

    private List<Instant> timesteps;

    public DispersionSimulation(String derivationIri) {
        this.derivationIri = derivationIri;
        pollutantIriToLabelMap = new HashMap<>();
        pollutantIriToDispLayerMap = new HashMap<>();
    }

    public void setScopeLabel(String label) {
        this.label = label;
    }

    public void setTimesteps(List<Long> timesteps) {
        if (!timesteps.isEmpty()) {
            this.timesteps = timesteps.stream().map(Instant::ofEpochSecond).collect(Collectors.toList());
        }
    }

    public List<Instant> getTimesteps() {
        return timesteps;
    }

    public String getLabel() {
        return label;
    }

    public String getDerivationIri() {
        return derivationIri;
    }

    public void setPollutantLabelAndDispLayer(String pollutant, String pollutantLabel, String dispLayer) {
        pollutantIriToLabelMap.put(pollutant, pollutantLabel);
        pollutantIriToDispLayerMap.put(pollutant, dispLayer);
    }

    public void removePollutant(String pollutant) {
        pollutantIriToDispLayerMap.remove(pollutant);
        pollutantIriToLabelMap.remove(pollutant);
    }

    public List<String> getPollutants() {
        return new ArrayList<>(pollutantIriToDispLayerMap.keySet());
    }

    public String getDispersionLayer(String pollutant) {
        return pollutantIriToDispLayerMap.get(pollutant);
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("time", timesteps);
        json.put("pollutants", pollutantIriToLabelMap);
        json.put("derivationIri", derivationIri);

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
}
