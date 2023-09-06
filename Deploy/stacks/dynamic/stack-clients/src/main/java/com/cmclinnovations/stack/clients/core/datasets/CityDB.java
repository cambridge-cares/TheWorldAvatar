package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.citydb.CityDBClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerClient;
import com.cmclinnovations.stack.clients.citydb.CityTilerOptions;
import com.cmclinnovations.stack.clients.citydb.ImpExpOptions;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;

public class CityDB extends GeoServerDataSubset {

    @JsonProperty
    private final ImpExpOptions importOptions = new ImpExpOptions();
    @JsonProperty
    private final CityTilerOptions cityTilerOptions = new CityTilerOptions();
    @JsonProperty
    private boolean append = true;
    @JsonProperty
    private boolean skipThematicSurfacesFudge = false;
    @JsonProperty
    private boolean usePreviousIRIs = true;
    @JsonProperty
    private Path previousFile;
    @JsonProperty
    private GeoServerVectorSettings geoServerSettings = new GeoServerVectorSettings();

    @JsonIgnore
    private String lineage;

    @Override
    void loadInternal(Dataset parent) {
        String database = parent.getDatabase();

        super.loadInternal(parent);

        writeOutPrevious(database);

        createLayer(parent.getWorkspaceName(), parent.getDatabase());

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
                    lineage, baseIRI, append);
        }

        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), database, importOptions, lineage, baseIRI,
                        append || usePreviousIRIs);

    }

    @Override
    public void createLayer(String workspaceName, String database) {
        GSVirtualTableEncoder virtualTable = geoServerSettings.getVirtualTable();
        if (null != virtualTable) {
            virtualTable.setSql(handleFileValues(virtualTable.getSql()));
        }

        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, getName(), geoServerSettings);
    }

    public void createLayer(String database) {

        long[] fudgedThematicSurfaceIDs = new long[0];

        if (!skipThematicSurfacesFudge) {
            fudgedThematicSurfaceIDs = CityDBClient.getInstance().applyThematicSurfacesFix(database);
        }

        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions);

        if (!skipThematicSurfacesFudge && 0 != fudgedThematicSurfaceIDs.length) {
            CityDBClient.getInstance().revertThematicSurfacesFix(database, fudgedThematicSurfaceIDs);
        }
    }

    private void writeOutPrevious(String database) {
        CityDBClient.getInstance().writeOutToCityGML(database, previousFile.toString(), lineage);
    }

}
