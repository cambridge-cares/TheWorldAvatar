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
    private List<DispXYZ> dispXYZs;

    private class DispXYZ {
        private String iri;
        private String pollutant;
        private int z;
        private List<Long> time;
        private List<String> output;

        private DispXYZ(String iri, String pollutant, int z) {
            this.iri = iri;
            this.pollutant = pollutant;
            this.z = z;
        }
    }

    public DispersionMetadata(String derivationIri) {
        this.derivationIri = derivationIri;
        dispXYZs = new ArrayList<>();
    }

    public void addDispXYZ(String iri, String pollutant, int z) {
        DispXYZ dispXYZ = new DispXYZ(iri,pollutant,z);
        dispXYZs.add(dispXYZ);
    }

    public List<String> getDispXYZsIRI() {
        ArrayList<String> listIRI = new ArrayList<>();
        for (DispXYZ dispXYZ : dispXYZs) {
            listIRI.add(dispXYZ.iri);
        }
        return listIRI;
    }

    public void updateDispXYZ(String dispersionXYZ, List<Long> simulationTimes, List<String> dispersionXYZfiles) {
        for (DispXYZ dispXYZ : dispXYZs) {
            if (Objects.equals(dispXYZ.iri, dispersionXYZ)) {
                dispXYZ.time = simulationTimes;
                dispXYZ.output = dispersionXYZfiles;
            }
        }
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

        JSONArray XYZJson = new JSONArray();

        for (DispXYZ dispXYZ : dispXYZs) {
            JSONObject jXYZ = new JSONObject();
            jXYZ.put("pollutant", dispXYZ.pollutant);
            jXYZ.put("height", dispXYZ.z);
            jXYZ.put("iri",dispXYZ.iri);
            jXYZ.put("time",dispXYZ.time);
            jXYZ.put("output",dispXYZ.output);
            XYZJson.put(jXYZ);
        }

        json.put("outputs",XYZJson);

        return json;
    }

    public void setScopePolygon(Polygon scopePolygon) {
        this.scopePolygon = scopePolygon;
    }

}
