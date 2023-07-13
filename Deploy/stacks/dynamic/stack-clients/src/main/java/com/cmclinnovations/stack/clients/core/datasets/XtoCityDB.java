package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.fasterxml.jackson.annotation.JsonProperty;

import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class XtoCityDB extends PostgresDataSubset {

    @JsonProperty
    private Ogr2OgrOptions ogr2ogrOptions = new Ogr2OgrOptions();
    @JsonProperty
    private ImpExpOptions importOptions = new ImpExpOptions();
    @JsonProperty
    private CityTilerOptions cityTilerOptions = new CityTilerOptions();
    @JsonProperty
    private GeoServerVectorSettings geoServerSettings = new GeoServerVectorSettings();

    @Override
    public boolean usesGeoServer() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        super.loadInternal(parent);
        CityDBClient.getInstance().addIRIs(parent.getDatabase());
        createLayer(parent.getDatabase());
        createLayer(parent.getWorkspaceName(), parent.getDatabase());
    }

    @Override
    public void loadData(Path dirPath, String database) {
        GDALClient.getInstance()
                .uploadVectorFilesToPostGIS(database, getTable(), dirPath.toString(), ogr2ogrOptions, false);
        CityDBClient.getInstance()
                .updateDatabase(database,importOptions.getSridIn());
    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

    public void createLayer(String workspaceName, String database) {
        GSVirtualTableEncoder virtualTable = geoServerSettings.getVirtualTable();
        if (null != virtualTable) {
            virtualTable.setSql(handleFileValues(virtualTable.getSql()));
        }

        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, getName(), geoServerSettings);
    }

}
