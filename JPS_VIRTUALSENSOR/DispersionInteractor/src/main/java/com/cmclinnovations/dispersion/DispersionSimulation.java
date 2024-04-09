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
    private String weatherStationIri;
    private Map<String, String> pollutantIriToLabelMap;
    private Map<String, String> pollutantIriToDispRasterMap;
    private Polygon scopePolygon;
    private Point weatherStationLocation;
    private Map<Integer, String> zMap;

    private List<Instant> timesteps;

    public DispersionSimulation(String derivationIri) {
        this.derivationIri = derivationIri;
        pollutantIriToLabelMap = new HashMap<>();
        pollutantIriToDispRasterMap = new HashMap<>();
        zMap = new HashMap<>();
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

    public void setPollutantLabelAndDispRaster(String pollutant, String pollutantLabel, String dispRaster) {
        pollutantIriToLabelMap.put(pollutant, pollutantLabel);
        pollutantIriToDispRasterMap.put(pollutant, dispRaster);
    }

    public void removePollutant(String pollutant) {
        pollutantIriToDispRasterMap.remove(pollutant);
        pollutantIriToLabelMap.remove(pollutant);
    }

    public List<String> getPollutants() {
        return new ArrayList<>(pollutantIriToDispRasterMap.keySet());
    }

    public String getDispersionRaster(String pollutant) {
        return pollutantIriToDispRasterMap.get(pollutant);
    }

    public JSONObject toJson() {
        JSONObject json = new JSONObject();
        json.put("time", timesteps);
        json.put("pollutants", pollutantIriToLabelMap);
        json.put("derivationIri", derivationIri);
        json.put("z", zMap);

        JSONObject weatherStation = new JSONObject();
        weatherStation.put("iri", weatherStationIri);
        weatherStation.put("wkt", weatherStationLocation.toString());

        json.put("weatherStation", weatherStation);

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

    public void setWeatherStationIri(String weatherStationIri) {
        this.weatherStationIri = weatherStationIri;
    }

    public String getWeatherStationIri() {
        return weatherStationIri;
    }

    public void setWeatherStationLocation(Point weatherStationLocation) {
        this.weatherStationLocation = weatherStationLocation;
    }

    public Point getWeatherStationLocation() {
        return weatherStationLocation;
    }

    public void addZValue(int zValue, String zIri) {
        zMap.put(zValue, zIri);
    }
}
