package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Files;
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
    @JsonProperty
    private boolean usePreviousIRIs = true;
    @JsonProperty
    private Path previousFile;

    @JsonIgnore
    private String lineage;

    @Override
    void loadInternal(Dataset parent) {
        String database = parent.getDatabase();

        super.loadInternal(parent);

        writeOutPrevious(database);

        if (!skipThematicSurfacesFix) {
            applyThematicSurfacesFix(database);
        }

        createLayer(database);
    }

    @Override
    public void loadData(Path dataSubsetDir, String database, String baseIRI) {

        lineage = dataSubsetDir.toString();

        if (null == previousFile) {
            previousFile = dataSubsetDir.resolveSibling(dataSubsetDir.getFileName() + "_previous")
                    .resolve("previous.gz");
        }

        usePreviousIRIs &= Files.exists(previousFile);
        if (usePreviousIRIs) {
            CityDBClient.getInstance().uploadFileToPostGIS(previousFile.toString(), database, importOptions,
                    lineage, baseIRI, false);
        }

        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), database, importOptions, lineage, baseIRI,
                        usePreviousIRIs);

    }

    public void createLayer(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);
    }

    private void applyThematicSurfacesFix(String database) {
        CityDBClient.getInstance().applyThematicSurfacesFix(database);
    }

    private void writeOutPrevious(String database) {
        CityDBClient.getInstance().writeOutToCityGML(database, previousFile.toString(), lineage);
    }

}
