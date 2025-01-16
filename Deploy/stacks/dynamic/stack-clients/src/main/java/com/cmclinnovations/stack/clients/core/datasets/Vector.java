package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Vector extends GeoServerDataSubset {

    @JsonProperty
    private Ogr2OgrOptions ogr2ogrOptions = new Ogr2OgrOptions();

    @JsonProperty
    private GeoServerVectorSettings geoServerSettings = new GeoServerVectorSettings();

    @Override
    public void loadData(Path dirPath, String database, String baseIRI) {
        GDALClient.getInstance()
                .uploadVectorFilesToPostGIS(database, getSchema(), getTable(), dirPath.toString(), ogr2ogrOptions,
                        false);
    }

    @Override
    public void createLayers(String workspaceName, String database) {
        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, getSchema(), getTable(), geoServerSettings);
    }

}
