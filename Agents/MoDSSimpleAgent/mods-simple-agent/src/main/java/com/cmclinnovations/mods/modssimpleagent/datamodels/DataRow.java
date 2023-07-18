package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.google.common.collect.Streams;

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
    @JsonIgnore
    public List<List<Double>> getRows() {
        return List.of(values);
    }

    @Override
    @JsonIgnore
    public List<DataColumn> getColumns() {
        return Streams.zip(headers.stream(), values.stream(),
                (header, value) -> new DataColumn(header, List.of(value)))
                .collect(Collectors.toList());
    }

}
