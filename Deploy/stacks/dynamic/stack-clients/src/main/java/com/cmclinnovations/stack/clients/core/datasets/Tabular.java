package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Tabular extends PostgresDataSubset {

    @JsonProperty
    private Ogr2OgrOptions ogr2ogrOptions = new Ogr2OgrOptions();

    public Ogr2OgrOptions getOptions() {
        return ogr2ogrOptions;
    }

    @Override
    public void loadData(Path dirPath, String database) {
        GDALClient.getInstance()
                .uploadVectorFilesToPostGIS(database, getTable(), dirPath.toString(), ogr2ogrOptions, false);
    }

}
