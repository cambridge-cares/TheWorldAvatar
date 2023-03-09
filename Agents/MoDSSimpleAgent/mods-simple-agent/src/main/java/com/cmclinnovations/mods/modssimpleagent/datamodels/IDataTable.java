package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.util.List;

public interface IDataTable {
    public List<String> getHeaders();

    public List<List<Double>> getRows();

    public List<DataColumn> getColumns();
}
