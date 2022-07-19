package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;

public class Raster extends DataSubset {

    private GDALTranslateOptions options = new GDALTranslateOptions();

    public GDALTranslateOptions getOptions() {
        return options;
    }

    @Override
    public void loadData(GDALClient gdalClient, String datasetDir, String database) {
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        gdalClient.uploadRasterFilesToPostGIS(database, getTable(), dirPath.toString(), options, false);
    }

    @Override
    public void createLayer(GeoServerClient geoServerClient, String dataSubsetDir, String workspaceName,
            String database) {
        geoServerClient.createGeoTiffLayer(workspaceName, getName(), "geoserver_raster_indicies", "public");
    }

}
