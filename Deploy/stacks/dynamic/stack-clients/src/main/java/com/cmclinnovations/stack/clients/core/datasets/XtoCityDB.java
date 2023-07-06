package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.fasterxml.jackson.annotation.JsonProperty;

public class XtoCityDB extends PostgresDataSubset {

    @JsonProperty
    private Ogr2OgrOptions ogr2ogrOptions = new Ogr2OgrOptions();
    @JsonProperty
    private ImpExpOptions importOptions = new ImpExpOptions();
    @JsonProperty
    private CityTilerOptions cityTilerOptions = new CityTilerOptions();

    @Override
    void loadInternal(Dataset parent) {
        super.loadInternal(parent);
        createLayer(parent.getDatabase());
    }

    @Override
    public void loadData(Path dirPath, String database) {
        CityDBClient.getInstance()
                .updateDatabase(database,importOptions.getSridIn());
        GDALClient.getInstance()
                .uploadVectorFilesToPostGIS(database, getTable(), dirPath.toString(), ogr2ogrOptions, false);
    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

}
