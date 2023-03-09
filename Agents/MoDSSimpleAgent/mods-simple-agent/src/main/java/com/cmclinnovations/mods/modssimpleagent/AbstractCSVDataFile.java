package com.cmclinnovations.mods.modssimpleagent;

import java.util.List;

import com.cmclinnovations.mods.modssimpleagent.datamodels.IDataTable;

public abstract class AbstractCSVDataFile implements FileGenerator {

    private final IDataTable data;

    protected AbstractCSVDataFile(IDataTable data) {
        this.data = data;
    }

    protected IDataTable getData() {
        return data;
    }

    public List<String> getHeaders() {
        return data.getHeaders();
    }

    public List<List<Double>> getRows() {
        return data.getRows();
    }
}
