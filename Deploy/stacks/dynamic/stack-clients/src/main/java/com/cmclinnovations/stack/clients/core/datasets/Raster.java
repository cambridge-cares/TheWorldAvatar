package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;

public class Raster extends DataSubset {

    private GDALTranslateOptions options = new GDALTranslateOptions();

    public GDALTranslateOptions getOptions() {
        return options;
    }

    @Override
    public void loadData(String datasetDir, String database) {
        GDALClient gdalClient = new GDALClient();
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        gdalClient.uploadRasterFilesToPostGIS(database, getTable(), dirPath.toString(), options, false);
    }

}
