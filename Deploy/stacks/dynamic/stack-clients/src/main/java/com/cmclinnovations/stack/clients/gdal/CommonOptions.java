package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

class CommonOptions<T extends CommonOptions<T>> {

    private String sridIn = null;
    private String sridOut = null;

    @JsonProperty
    private final Map<String, String> inputDatasetOpenOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> envVars = new HashMap<>();
    @JsonProperty
    private final Map<String, List<String>> otherOptions = new HashMap<>();

    protected CommonOptions() {
    }

    public T setSridIn(String sridIn) {
        this.sridIn = sridIn;
        return (T) this;
    }

    public T setSridOut(String sridOut) {
        this.sridOut = sridOut;
        return (T) this;
    }

    public T addInputDatasetOpenOption(String name, String value) {
        inputDatasetOpenOptions.put(name, value);
        return (T) this;
    }

    public T addOtherOption(String option, String... values) {
        otherOptions.put(option, Arrays.asList(values));
        return (T) this;
    }

    public T withEnv(String key, String value) {
        envVars.put(key, value);
        return (T) this;
    }

    public Map<String, String> getEnv() {
        return envVars;
    }

    protected List<String> appendCommonToArgs(String... args) {
        List<String> allArgs = new ArrayList<>(args.length);
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

        inputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-oo", name, value));

        otherOptions.forEach((option, values) -> {
            allArgs.add(option);
            allArgs.addAll(values);
        });
        return allArgs;
    }

    protected void addKeyValuePair(List<String> allArgs, String option, String name, String value) {
        allArgs.add(option);
        allArgs.add(name + "=" + value);
    }
}