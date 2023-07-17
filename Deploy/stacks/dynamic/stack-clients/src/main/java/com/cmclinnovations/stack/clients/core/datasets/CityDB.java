package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

public class CityDB extends PostgresDataSubset {

    @JsonProperty
    private final ImpExpOptions importOptions = new ImpExpOptions();
    @JsonProperty
    private final CityTilerOptions cityTilerOptions = new CityTilerOptions();
    @JsonProperty
    private boolean skipThematicSurfacesFix = false;

    @JsonIgnore
    private String lineage;

    @Override
    void loadInternal(Dataset parent) {
        super.loadInternal(parent);
        if (!skipThematicSurfacesFix) {
            applyThematicSurfacesFix(parent.getDatabase());
        }
        createLayer(parent.getDatabase());
    }

    @Override
    public void loadData(Path dataSubsetDir, String database) {

        lineage = dataSubsetDir.toString();

        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), database, importOptions, lineage, false);

    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

    private void applyThematicSurfacesFix(String database) {
        CityDBClient.getInstance().applyThematicSurfacesFix(database);
    }

}
