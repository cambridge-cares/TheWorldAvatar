package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.Iterator;
import java.util.List;

public class DataRow implements IDataTable {
    private final List<String> headers;
    private final List<Double> values;

    public DataRow(List<String> headers, List<Double> values) {
        this.headers = headers;
        this.values = values;
    }

    @Override
    public List<String> getHeaders() {
        return headers;
    }

    public List<Double> getValues() {
        return values;
    }

    @Override
    public List<List<Double>> getValuesMatrix() {
        return List.of(values);
    }

    public Iterator<List<Double>> getRowIterator() {
        return List.of(values).iterator();
    }

    public Iterator<List<Double>> getColumnIterator() {
        return values.stream().map(List::of).iterator();
    }

}
