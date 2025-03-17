package uk.ac.cam.cares.jps.agent.sensorloggermobileappagent.model;

import java.util.ArrayList;
import java.util.List;

public class SensorData<E> {
    private String dataIri;
    private boolean needToInitTimeSeries = false;
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

    public String getDataIri() {
        return dataIri;
    }

    public boolean isNeedToInitTimeSeries() {
        return needToInitTimeSeries;
    }

    public List<E> getValues() {
        return values;
    }

    public Class<E> getType() {
        return type;
    }

    public void setDataIri(String iri) {
        this.dataIri = iri;
    }

    public void setNeedToInitTimeSeries(boolean needToInitTimeSeries) {
        this.needToInitTimeSeries = needToInitTimeSeries;
    }
}
