package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

public class Tabular extends DataSubset {

    private Ogr2OgrOptions options = new Ogr2OgrOptions();

    public Ogr2OgrOptions getOptions() {
        return options;
    }

    @Override
    public void loadData(GDALClient gdalClient, String datasetDir, String database) {
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        gdalClient.uploadVectorFilesToPostGIS(database, getTable(), dirPath.toString(), options, false);
    }

    @Override
    public void createLayer(GeoServerClient geoServerClient, String dataSubsetDir, String workspaceName,
            String database) {
        // Don't need to do anything
    }

}
