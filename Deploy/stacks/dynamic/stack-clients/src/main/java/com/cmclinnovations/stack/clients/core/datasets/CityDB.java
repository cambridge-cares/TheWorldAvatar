package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.fasterxml.jackson.annotation.JsonProperty;

public class CityDB extends DataSubset {

    @JsonProperty
    private ImpExpOptions importOptions = new ImpExpOptions();
    @JsonProperty
    private CityTilerOptions cityTilerOptions = new CityTilerOptions();

    @Override
    public boolean usesPostGIS() {
        return true;
    }

    @Override
    void loadInternal(Dataset parent) {
        Path dataSubsetDir = parent.getDirectory().resolve(this.getSubdirectory());
        String database = parent.getDatabase();
        loadData(dataSubsetDir, database);
        createLayer(database);
    }

    public void loadData(Path dataSubsetDir, String database) {
        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), database, importOptions, false);

    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

}
