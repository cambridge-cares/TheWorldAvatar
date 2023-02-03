package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions.Subcommand;
import com.fasterxml.jackson.annotation.JsonProperty;

public class CityDB extends DataSubset {

    @JsonProperty
    private ImpExpOptions importOptions = new ImpExpOptions(Subcommand.IMPORT);
    private CityTilerOptions cityTilerOptions = new CityTilerOptions();

    @Override
    void loadInternal(Dataset parent) {
        Path dataSubsetDir = parent.getDirectory().resolve(this.getSubdirectory());
        loadData(dataSubsetDir);
        createLayer(parent.getDatabase());
    }

    public void loadData(Path dataSubsetDir) {
        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), importOptions, false);

    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

}
