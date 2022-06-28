package com.cmclinnovations.stack;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.stack.clients.gdal.GDALClient;
import com.cmclinnovations.stack.clients.gdal.Ogr2OgrOptions;
import com.cmclinnovations.stack.clients.postgis.PostGISClient;

public class TempTestCalls {

    private TempTestCalls() {
    }

    static void doStuff() {
        String databaseName = "test_database";
        String filePath = "/inputs/data/031WAF112.json";

        PostGISClient postGISClient = new PostGISClient();
        try {

            postGISClient.createDatabase(databaseName);
            postGISClient.createDatabase(databaseName);

            String fileContents = Files.readString(Path.of(filePath));
            GDALClient gdalClient = new GDALClient();
            gdalClient.uploadVectorStringToPostGIS(databaseName, "layer_from_string",
                    fileContents, new Ogr2OgrOptions().setSridIn("EPSG:4326"));

            gdalClient.uploadVectorFileToPostGIS(databaseName, "layer_from_file",
                    filePath, new Ogr2OgrOptions());

            gdalClient.uploadVectorURLToPostGIS(databaseName, "layer_from_url",
                    "http://environment.data.gov.uk/flood-monitoring/id/floodAreas/031WAF112/polygon",
                    new Ogr2OgrOptions().setSridIn("EPSG:4326").setSridOut("EPSG:27700"));

            postGISClient.removeDatabase(databaseName);
            postGISClient.removeDatabase(databaseName);
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
}
