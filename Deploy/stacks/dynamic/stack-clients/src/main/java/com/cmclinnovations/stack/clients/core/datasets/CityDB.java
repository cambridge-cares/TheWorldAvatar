package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Path;

import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions.Subcommand;
import com.fasterxml.jackson.annotation.JsonProperty;

public class CityDB extends DataSubset {

    @JsonProperty
    private ImpExpOptions importOptions = new ImpExpOptions(Subcommand.IMPORT);

    @Override
    public void loadData(String datasetDir, String database) {
        Path dirPath = Path.of(datasetDir, getSubdirectory());
        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dirPath.toString(), importOptions, false);
    }

    @Override
    public void createLayer(String dataSubsetDir, String workspaceName, String database) {
    }

}
