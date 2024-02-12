package com.cmclinnovations.stack;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.core.datasets.DatasetLoader;
import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.GDALTranslateOptions;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.geoserver.MultidimSettings;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class TempTestCalls {

    private TempTestCalls() {
    }

    static void doStuff() {

        DatasetLoader.uploadInputDatasets();

        GDALClient gdalClient = GDALClient.getInstance();
        PostGISClient postGISClient = PostGISClient.getInstance();

        String rasterDatabase = "rasters";
        postGISClient.createDatabase(rasterDatabase);
        gdalClient.uploadRasterFilesToPostGIS(rasterDatabase, PostGISClient.DEFAULT_SCHEMA_NAME, "elevation", "/inputs/data/rasters",
                new GDALTranslateOptions(), new MultidimSettings(), false);
        String databaseName = "test_database";
        String filePath = "/inputs/data/031WAF112.json";

        try {
            postGISClient.createDatabase(databaseName);
            postGISClient.createDatabase(databaseName);

            String fileContents = Files.readString(Path.of(filePath));
            gdalClient.uploadVectorStringToPostGIS(databaseName, "layer_from_string",
                    fileContents, new Ogr2OgrOptions().setSridIn("EPSG:4326"), false);

            gdalClient.uploadVectorFileToPostGIS(databaseName, "layer_from_file",
                    filePath, new Ogr2OgrOptions(), true);

            gdalClient.uploadVectorURLToPostGIS(databaseName, "layer_from_url",
                    "http://environment.data.gov.uk/flood-monitoring/id/floodAreas/031WAF112/polygon",
                    new Ogr2OgrOptions().setSridIn("EPSG:4326").setSridOut("EPSG:27700"), false);

            postGISClient.removeDatabase(rasterDatabase);
            postGISClient.removeDatabase(rasterDatabase);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
