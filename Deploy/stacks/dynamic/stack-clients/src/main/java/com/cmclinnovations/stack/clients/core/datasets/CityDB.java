package com.cmclinnovations.stack.clients.core.datasets;

import java.nio.file.Files;
import java.nio.file.Path;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

    private static final Logger logger = LoggerFactory.getLogger(CityDB.class);

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
    @JsonProperty
    private boolean augmentData = true;
    @JsonProperty
    private boolean discoverThematicSurface = false;
    @JsonProperty
    private double critAreaRatio = 0.1;
    @JsonProperty
    private boolean createTile = true;
    @JsonProperty
    private boolean parallelTiling = true;

    @JsonIgnore
    private String lineage;

    protected String getSridIn() {
        return importOptions.getSridIn();
    }

    protected CityTilerOptions getCityTilerOptions() {
        return cityTilerOptions;
    }

    protected void setPreviousFile(Path previousFilePath) {
        previousFile = previousFilePath;
    }

    @Override
    void loadInternal(Dataset parent) {
        String database = parent.getDatabase();

        super.loadInternal(parent);

        if (null != previousFile) {
            logger.info("Exporting data...");
            writeOutPrevious(database);
        }

        if (createTile) {
            logger.info("Creating 3D tiles...");
            createLayer(database);
        }

    }

    @Override
    public void loadData(Path dataSubsetDir, String database, String baseIRI) {

        lineage = dataSubsetDir.toString();

        loadDataInternal(dataSubsetDir, database, baseIRI, lineage);

    }

    protected void loadDataInternal(Path dataSubsetDir, String database, String baseIRI, String lineage) {
        if (null == previousFile) {
            previousFile = dataSubsetDir.resolveSibling(dataSubsetDir.getFileName() + "_previous")
                    .resolve("previous.gz");
        }

        usePreviousIRIs &= Files.exists(previousFile);
        if (usePreviousIRIs) {
            logger.info("Loading previous data...");
            CityDBClient.getInstance().uploadFileToPostGIS(previousFile.toString(), database, importOptions,
                    lineage, baseIRI, append);
        }

        logger.info("Uploading data...");

        CityDBClient.getInstance()
                .uploadFilesToPostGIS(dataSubsetDir.toString(), database, importOptions, lineage, baseIRI,
                        append || usePreviousIRIs);
    }

    protected void augmentData(String database) {
        
        logger.info("Setting tables to unlogged for better write performance...");
        CityDBClient.getInstance().unlogTable(database);

        if (discoverThematicSurface) {
            logger.info("Discovering thematic surface...");
            CityDBClient.getInstance().discoverThematicSurface(database, critAreaRatio);
        }
        logger.info("Adding building height...");
        CityDBClient.getInstance().addBuildingHeight(database);
        logger.info("Adding building footprint...");
        CityDBClient.getInstance().addFootprint(database);

        logger.info("Setting tables to logged...");
        CityDBClient.getInstance().relogTable(database);
    }

    @Override
    public void runSQLPostProcess(String database) {

        if (augmentData) {
            augmentData(database);
        }
        super.runSQLPostProcess(database);
    }

    @Override
    public void createLayers(String workspaceName, String database) {

        logger.info("Publishing to geoserver...");

        GSVirtualTableEncoder virtualTable = geoServerSettings.getVirtualTable();
        if (null != virtualTable) {
            virtualTable.setSql(handleFileValues(virtualTable.getSql()));
        }

        GeoServerClient.getInstance()
                .createPostGISLayer(workspaceName, database, getName(), geoServerSettings);
    }

    public void createLayer(String database) {

        long[] shiftedThematicSurfaceIDs = new long[0];
        long[] fudgedThematicSurfaceIDs = new long[0];

        if (!skipThematicSurfacesFudge) {
            shiftedThematicSurfaceIDs = CityDBClient.getInstance().applyThematicSurfacesShift(database);
            fudgedThematicSurfaceIDs = CityDBClient.getInstance().applyThematicSurfacesFix(database);
        }

        generateTiles(database);

        if (!skipThematicSurfacesFudge && 0 != fudgedThematicSurfaceIDs.length) {
            CityDBClient.getInstance().revertThematicSurfacesFix(database, fudgedThematicSurfaceIDs);
        }
        if (!skipThematicSurfacesFudge && 0 != shiftedThematicSurfaceIDs.length) {
            CityDBClient.getInstance().revertThematicSurfacesShift(database, shiftedThematicSurfaceIDs);
        }
    }

    protected void generateTiles(String database) {
        CityTilerClient.getInstance().generateTiles(database, "citydb", cityTilerOptions, parallelTiling);
    }

    private void writeOutPrevious(String database) {
        CityDBClient.getInstance().writeOutToCityGML(database, previousFile.toString(), lineage);
    }

}
