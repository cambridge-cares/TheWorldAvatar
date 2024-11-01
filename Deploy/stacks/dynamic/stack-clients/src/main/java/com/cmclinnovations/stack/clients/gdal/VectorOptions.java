package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class VectorOptions<T extends VectorOptions<T>> extends CommonOptions<T> {

    @JsonProperty
    private final Map<String, String> datasetCreationOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> layerCreationOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> outputDatasetOpenOptions = new HashMap<>();

    public VectorOptions(String commamnd) {
        super(commamnd);
    }

    @SuppressWarnings("unchecked")
    public T addDatasetCreationOption(String name, String value) {
        datasetCreationOptions.put(name, value);
        return (T) this;
    }

    @SuppressWarnings("unchecked")
    public T addLayerCreationOption(String name, String value) {
        layerCreationOptions.put(name, value);
        return (T) this;
    }

    @SuppressWarnings("unchecked")
    public T addOutputDatasetOpenOption(String name, String value) {
        outputDatasetOpenOptions.put(name, value);
        return (T) this;
    }

    private void processDatasetCreationOption(List<String> args, String name, String value) {
        processKeyValuePair(args, "-dsco", name, value);
    }

    private void processLayerCreationOption(List<String> args, String name, String value) {
        processKeyValuePair(args, "-lco", name, value);
    }

    private void processOutputDatasetOpenOption(List<String> args, String name, String value) {
        processKeyValuePair(args, "-doo", name, value);
    }

    public String[] generateCommand(String layerName, boolean append, String source, String destination,
            String... extraArgs) {

        List<String> args = new ArrayList<>(2 * extraArgs.length);

        if (null != layerName) {
            processLayerNameOption(layerName, args);
        }

        processConfigOption(args, "OGR_TRUNCATE", append ? "NO" : "YES");

        return generateCommandInternal(args, source, destination, extraArgs);
    }

    protected void processLayerNameOption(String layerName, List<String> args) {
        processOtherOption(args, "-nln", layerName);
    }

    @Override
    protected void processArgs(List<String> args) {
        super.processArgs(args);

        processOtherOption(args, "-f", "PostgreSQL");
    }

    public void setSchema(String schema) {
        if (null != schema) {
            layerCreationOptions.put("SCHEMA", schema);
        }
    }
}