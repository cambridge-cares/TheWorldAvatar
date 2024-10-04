package uk.ac.cam.cares.jps.agent.ceavisualisation;

import java.util.Map;

class VisValues {
    public String iri;
    public Map<String, Double> values;

    VisValues(String iri, Map<String, Double> values) {
        this.iri = iri;
        this.values = values;
    }

    void setIri(String iri) {
        this.iri = iri;
    }

    String getIri() {
        return this.iri;
    }

    void setValues(Map<String, Double> values) {
        this.values = values;
    }

    Map<String, Double> getValues() {
        return this.values;
    }
}
