package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.Iterator;
import java.util.List;

public interface IDataTable extends Iterable<List<Double>> {
    public List<String> getHeaders();

    public List<List<Double>> getValuesMatrix();

    public default Iterator<List<Double>> iterator() {
        return getRowIterator();
    }

    public Iterator<List<Double>> getRowIterator();

    public Iterator<List<Double>> getColumnIterator();
}
