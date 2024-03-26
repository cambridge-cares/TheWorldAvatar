package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerRasterSettings;
import com.cmclinnovations.stack.clients.geoserver.MultidimSettings;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Raster extends GeoServerDataSubset {

    @JsonProperty
    private GDALTranslateOptions gdalTranslateOptions = new GDALTranslateOptions();

    @JsonProperty
    private GeoServerRasterSettings geoServerSettings = new GeoServerRasterSettings();

    @JsonProperty
    private MultidimSettings mdimSettings;

    @Override
    public void loadData(Path dirPath, String database, String baseIRI) {
        GDALClient.getInstance()
                .uploadRasterFilesToPostGIS(database, PostGISClient.DEFAULT_SCHEMA_NAME, getTable(), dirPath.toString(),
                        gdalTranslateOptions, mdimSettings, false);
    }

    @Override
    public void createLayers(String workspaceName, String database) {
        GeoServerClient.getInstance()
                .createGeoTiffLayer(workspaceName, getName(), database, PostGISClient.DEFAULT_SCHEMA_NAME,
                        geoServerSettings, mdimSettings);
    }

}
