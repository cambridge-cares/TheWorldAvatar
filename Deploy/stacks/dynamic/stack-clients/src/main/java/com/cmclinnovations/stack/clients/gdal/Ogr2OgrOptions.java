package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
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

    public Ogr2OgrOptions() {
        super("ogr2ogr");
    }

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
            processOtherOption(args, "-nln", layerName);
        }

        processConfigOption(args, "OGR_TRUNCATE", append ? "NO" : "YES");

        return generateCommandInternal(args, source, destination, extraArgs);
    }

    /**
     * ogr2ogr needs the destination before the source.
     */
    @Override
    protected void processSourceAndDestination(String source, String destination, List<String> args) {
        args.add(destination);
        args.add(source);
    }

    @Override
    protected void processArgs(List<String> args) {
        super.processArgs(args);

        processOtherOption(args, "-f", "PostgreSQL");

        processConfigOption(args, "PG_USE_COPY", "YES");

        // Setting this option prevents GDAL from "cleaning" the table and column
        // names for Postgres, as described here:
        // https://gdal.org/drivers/vector/pg.html#layer-creation-options
        processLayerCreationOption(args, "LAUNDER", "NO");

        datasetCreationOptions.forEach((name, value) -> processDatasetCreationOption(args, name, value));
        layerCreationOptions.forEach((name, value) -> processLayerCreationOption(args, name, value));
        outputDatasetOpenOptions.forEach((name, value) -> processOutputDatasetOpenOption(args, name, value));
    }

    public void setSchema(String schema) {
        if (null != schema) {
            layerCreationOptions.put("SCHEMA", schema);
        }
    }
}