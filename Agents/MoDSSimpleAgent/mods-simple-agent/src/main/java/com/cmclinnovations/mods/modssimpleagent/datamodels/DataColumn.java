package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

public class DataColumn implements IDataTable {

    private final String name;
    private final List<Double> values;

    private DataColumn() {
        name = null;
        values = null;
    }

    public DataColumn(String name, List<Double> values) {
        this.name = name;
        this.values = values;
    }

    public String getName() {
        return name;
    }

    public List<Double> getValues() {
        return values;
    }

    @Override
    public List<String> getHeaders() {
        return List.of(name);
    }

    @Override
    public List<List<Double>> getValuesMatrix() {
        return values.stream().map(List::of).collect(Collectors.toList());
    }

    @Override
    public Iterator<List<Double>> getRowIterator() {
        return values.stream().map(List::of).iterator();
    }

    @Override
    public Iterator<List<Double>> getColumnIterator() {
        return List.of(values).iterator();
    }
}
