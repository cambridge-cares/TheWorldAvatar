package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.ArrayList;
import java.util.Collections;
import java.util.DoubleSummaryStatistics;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonValue;

public class Data implements IDataTable {

    @JsonValue
    private List<DataColumn> columns;

    @JsonIgnore
    List<DoubleSummaryStatistics> stats;

    @JsonCreator
    public Data(List<DataColumn> columns) {
        this.columns = columns;
    }

    @Override
    public List<DataColumn> getColumns() {
        return columns;
    }

    public void setColumns(List<DataColumn> columns) {
        this.columns = columns;
    }

    @Override
    @JsonIgnore
    public List<String> getHeaders() {
        return columns.stream().map(DataColumn::getName).collect(Collectors.toUnmodifiableList());
    }

    @Override
    @JsonIgnore
    public List<List<Double>> getRows() {
        if (columns.isEmpty()) {
            return Collections.emptyList();
        }

        List<List<Double>> rows = IntStream.range(0, columns.size())
                .mapToObj(i -> new ArrayList<Double>()).collect(Collectors.toList());

        columns.stream().map(DataColumn::getValues)
                .forEachOrdered(column -> {
                    Iterator<List<Double>> iterator = rows.iterator();
                    column.forEach(value -> iterator.next().add(value));
                });
        return rows;
    }

    @JsonIgnore
    private List<DoubleSummaryStatistics> getStats() {
        if (null == stats) {
            stats = columns.stream()
                    .map(column -> column.getValues().stream()
                            .collect(Collectors.summarizingDouble(Number::doubleValue)))
                    .collect(Collectors.toList());
        }
        return stats;
    }

    @JsonIgnore
    public DataRow getAverages() {
        return new DataRow(getHeaders(), getStats().stream()
                .map(DoubleSummaryStatistics::getAverage)
                .collect(Collectors.toList()));
    }

    @JsonIgnore
    public DataRow getMinimums() {
        return new DataRow(getHeaders(), getStats().stream()
                .map(DoubleSummaryStatistics::getMin)
                .collect(Collectors.toList()));
    }

    @JsonIgnore
    public DataRow getMaximums() {
        return new DataRow(getHeaders(), getStats().stream()
                .map(DoubleSummaryStatistics::getMax)
                .collect(Collectors.toList()));
    }

}
