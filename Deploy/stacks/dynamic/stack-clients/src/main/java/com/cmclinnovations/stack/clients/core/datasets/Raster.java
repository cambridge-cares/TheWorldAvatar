package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerRasterSettings;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Raster extends DataSubset {

    @JsonProperty
    private GDALTranslateOptions options = new GDALTranslateOptions();

    @JsonProperty
    private GeoServerRasterSettings geoServerSettings = new GeoServerRasterSettings();

    @Override
    public void loadData(String datasetDir, String database) {
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        GDALClient.getInstance()
                .uploadRasterFilesToPostGIS(database, getTable(), dirPath.toString(), options, false);
    }

    @Override
    public void createLayer(String dataSubsetDir, String workspaceName,
            String database) {
        GeoServerClient.getInstance()
                .createGeoTiffLayer(workspaceName, getName(), "geoserver_raster_indicies", "public", geoServerSettings);
    }

}
