package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;

public class Vector extends DataSubset {

    private Ogr2OgrOptions options = new Ogr2OgrOptions();

    public Ogr2OgrOptions getOptions() {
        return options;
    }

    @Override
    public void loadData(String datasetDir, String database) {
        GDALClient gdalClient = new GDALClient();
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        gdalClient.uploadVectorFilesToPostGIS(database, getTable(), dirPath.toString(), options, false);

    }

}
