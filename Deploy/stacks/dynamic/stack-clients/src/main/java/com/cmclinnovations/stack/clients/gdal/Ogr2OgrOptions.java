package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Ogr2OgrOptions {

    private String sridIn = null;
    private String sridOut = null;

    private final List<String> datasetCreationOptions = new ArrayList<>();
    private final List<String> layerCreationOptions = new ArrayList<>();
    private final List<String> inputDatasetOpenOptions = new ArrayList<>();
    private final List<String> outputDatasetOpenOptions = new ArrayList<>();

    private final Map<String, String> envVars = new HashMap<>();

    public Ogr2OgrOptions setSridIn(String sridIn) {
        this.sridIn = sridIn;
        return this;
    }

    public Ogr2OgrOptions setSridOut(String sridOut) {
        this.sridOut = sridOut;
        return this;
    }

    public Ogr2OgrOptions addDatasetCreationOption(String name, String value) {
        datasetCreationOptions.add(name + '=' + value);
        return this;
    }

    public Ogr2OgrOptions addLayerCreationOption(String name, String value) {
        layerCreationOptions.add(name + '=' + value);
        return this;
    }

    public Ogr2OgrOptions addInputDatasetOpenOption(String name, String value) {
        inputDatasetOpenOptions.add(name + '=' + value);
        return this;
    }

    public Ogr2OgrOptions addOutputDatasetOpenOption(String name, String value) {
        outputDatasetOpenOptions.add(name + '=' + value);
        return this;
    }

    public Ogr2OgrOptions withEnv(String key, String value) {
        envVars.put(key, value);
        return this;
    }

    public Map<String, String> getEnv() {
        return envVars;
    }

    String[] appendToArgs(String... args) {
        List<String> allArgs = new ArrayList<>();
        Collections.addAll(allArgs, args);

        if (null != sridOut) {
            allArgs.add("-t_srs");
            allArgs.add(sridOut);
            if (null != sridIn) {
                allArgs.add("-s_srs");
                allArgs.add(sridIn);
            }
        } else {
            if (null != sridIn) {
                allArgs.add("-a_srs");
                allArgs.add(sridIn);
            }
        }

        datasetCreationOptions.forEach(value -> addKeyValuePair(allArgs, "-dsco", value));
        layerCreationOptions.forEach(value -> addKeyValuePair(allArgs, "-lco", value));
        inputDatasetOpenOptions.forEach(value -> addKeyValuePair(allArgs, "-oo", value));
        outputDatasetOpenOptions.forEach(value -> addKeyValuePair(allArgs, "-doo", value));

        return allArgs.toArray(args);
    }

    private void addKeyValuePair(List<String> allArgs, String key, String value) {
        allArgs.add(key);
        allArgs.add(value);
    }
}