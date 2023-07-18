package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;

public class DataColumn implements IDataTable {

    private final String name;
    private final List<Double> values;

    DataColumn() {
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
    @JsonIgnore
    public List<String> getHeaders() {
        return List.of(name);
    }

    @Override
    @JsonIgnore
    public List<List<Double>> getRows() {
        return values.stream().map(List::of).collect(Collectors.toList());
    }

    @Override
    @JsonIgnore
    public List<DataColumn> getColumns() {
        return List.of(this);
    }
}
