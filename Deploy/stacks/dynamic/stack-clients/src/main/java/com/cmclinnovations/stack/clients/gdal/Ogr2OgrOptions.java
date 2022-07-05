package com.cmclinnovations.stack.clients.gdal;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Ogr2OgrOptions extends CommonOptions<Ogr2OgrOptions> {

    private final Map<String, String> datasetCreationOptions = new HashMap<>();
    private final Map<String, String> layerCreationOptions = new HashMap<>();
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

    String[] appendToArgs(String... args) {
        List<String> allArgs = appendCommonToArgs(args);

        datasetCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-dsco", name, value));
        layerCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-lco", name, value));
        outputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-doo", name, value));

        return allArgs.toArray(args);
    }
}