package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALPolygonizeOptions;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.gdal.VectorOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Vector extends GeoServerDataSubset {

    @JsonProperty
    private VectorOptions<?> vectorOptions = new Ogr2OgrOptions();
    @JsonProperty
    private GeoServerVectorSettings geoServerSettings = new GeoServerVectorSettings();

    @Override
    public void loadData(Path dirPath, String database, String baseIRI) {
        GDALClient.getInstance()
                .uploadVectorFilesToPostGIS(database, getSchema(), getTable(), dirPath.toString(), vectorOptions,
                        false);
    }

    @Override
    public void createLayers(String workspaceName, String database) {
        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, getSchema(), getTable(), geoServerSettings);
    }

    @JsonProperty("Ogr2OgrOptions")
    void setOgr2Ogroptions(Ogr2OgrOptions ogr2OgrOptions) {
        vectorOptions = ogr2OgrOptions;
    }

    @JsonProperty("gdalPolygonizeOptions")
    void gDALPolygonizeOptions(GDALPolygonizeOptions gDALWarpOptions) {
        vectorOptions = gDALWarpOptions;
    }

}
