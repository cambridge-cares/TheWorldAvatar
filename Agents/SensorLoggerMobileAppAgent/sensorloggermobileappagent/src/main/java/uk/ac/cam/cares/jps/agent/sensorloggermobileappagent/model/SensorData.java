package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model;

import java.util.ArrayList;
import java.util.List;

public class SensorData<E> {
    private String iri;
    private final List<E> values = new ArrayList<>();
    private final Class<E> type;

    public SensorData(Class<E> type) {
        this.type = type;
    }

    public void addData(List<E> newValues) {
        values.addAll(newValues);
    }

    public void clearData() {
        values.clear();
    }

    public String getIri() {
        return iri;
    }

    public List<E> getValues() {
        return values;
    }

    public Class<E> getType() {
        return type;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }
}
