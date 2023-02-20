package com.cmclinnovations.stack.clients.gdal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Ogr2OgrOptions extends CommonOptions<Ogr2OgrOptions> {

    @JsonProperty
    private final Map<String, String> datasetCreationOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> layerCreationOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> outputDatasetOpenOptions = new HashMap<>();

    public Ogr2OgrOptions addDatasetCreationOption(String name, String value) {
        datasetCreationOptions.put(name, value);
        return this;
    }

    public Ogr2OgrOptions addLayerCreationOption(String name, String value) {
        layerCreationOptions.put(name, value);
        return this;
    }

    public Ogr2OgrOptions addOutputDatasetOpenOption(String name, String value) {
        outputDatasetOpenOptions.put(name, value);
        return this;
    }

    String[] appendToArgs(String layerName, String... args) {

        // Setting this option prevents GDAL from "cleaning" the table and column
        // names for Postgres, as described here:
        // https://gdal.org/drivers/vector/pg.html#layer-creation-options
        layerCreationOptions.put("LAUNDER", "NO");

        List<String> allArgs = appendCommonToArgs(args);
        if (null != layerName) {
            allArgs.add("-nln");
            allArgs.add(layerName);
        }
        datasetCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-dsco", name, value));
        layerCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-lco", name, value));
        outputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-doo", name, value));

        return allArgs.toArray(args);
    }
}