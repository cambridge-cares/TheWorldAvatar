package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALOptions;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.gdal.GDALWarpOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerRasterSettings;
import com.cmclinnovations.stack.clients.geoserver.MultidimSettings;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Raster extends GeoServerDataSubset {

    @JsonProperty
    GDALOptions<?> gdalOptions = new GDALTranslateOptions();

    @JsonProperty
    private GeoServerRasterSettings geoServerSettings = new GeoServerRasterSettings();

    @JsonProperty
    private MultidimSettings mdimSettings;

    @Override
    public void loadData(Path dirPath, String database, String baseIRI) {
        GDALClient.getInstance()
                .uploadRasterFilesToPostGIS(database, getSchema(), getTable(), dirPath.toString(),
                        gdalOptions, mdimSettings, false);
    }

    @Override
    public void createLayers(String workspaceName, String database) {
        GeoServerClient.getInstance()
                .createGeoTiffLayer(workspaceName, getTable(), database, getSchema(),
                        geoServerSettings, mdimSettings);
    }

    @JsonProperty("gdalTranslateOptions")
    void setGDALTranslateOptions(GDALTranslateOptions gdalTranslateOptions) {
        gdalOptions = gdalTranslateOptions;
    }

    @JsonProperty("gdalWarpOptions")
    void setGDALWarpOptions(GDALWarpOptions gDALWarpOptions) {
        gdalOptions = gDALWarpOptions;
    }
}
