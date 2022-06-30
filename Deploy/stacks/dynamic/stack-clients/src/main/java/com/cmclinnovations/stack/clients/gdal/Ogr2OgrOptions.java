package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Ogr2OgrOptions {

    private String sridIn = null;
    private String sridOut = null;

    private final Map<String, String> datasetCreationOptions = new HashMap<>();
    private final Map<String, String> layerCreationOptions = new HashMap<>();
    private final Map<String, String> inputDatasetOpenOptions = new HashMap<>();
    private final Map<String, String> outputDatasetOpenOptions = new HashMap<>();

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
        datasetCreationOptions.put(name, value);
        return this;
    }

    public Ogr2OgrOptions addLayerCreationOption(String name, String value) {
        layerCreationOptions.put(name, value);
        return this;
    }

    public Ogr2OgrOptions addInputDatasetOpenOption(String name, String value) {
        inputDatasetOpenOptions.put(name, value);
        return this;
    }

    public Ogr2OgrOptions addOutputDatasetOpenOption(String name, String value) {
        outputDatasetOpenOptions.put(name, value);
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

        datasetCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-dsco", name, value));
        layerCreationOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-lco", name, value));
        inputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-oo", name, value));
        outputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-doo", name, value));

        return allArgs.toArray(args);
    }

    private void addKeyValuePair(List<String> allArgs, String option, String name, String value) {
        allArgs.add(option);
        allArgs.add(name + "=" + value);
    }
}