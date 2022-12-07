package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerRasterSettings;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Raster extends DataSubset {

    @JsonProperty
    private GDALTranslateOptions gdalTranslateOptions = new GDALTranslateOptions();

    @JsonProperty
    private GeoServerRasterSettings geoServerSettings = new GeoServerRasterSettings();

    @Override
    public void loadData(GDALClient gdalClient, String datasetDir, String database) {
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        gdalClient.uploadRasterFilesToPostGIS(database, getTable(), dirPath.toString(), gdalTranslateOptions, false);
    }

    @Override
    public void createLayer(GeoServerClient geoServerClient, String dataSubsetDir, String workspaceName,
            String database) {
        geoServerClient.createGeoTiffLayer(workspaceName, getName(), "geoserver_raster_indicies", "public",
                geoServerSettings);
    }

}
