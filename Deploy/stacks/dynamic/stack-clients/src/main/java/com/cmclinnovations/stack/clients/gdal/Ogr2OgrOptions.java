package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

public class Ogr2OgrOptions extends VectorOptions<Ogr2OgrOptions> {

    public Ogr2OgrOptions() {
        super("ogr2ogr");
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

        processConfigOption(args, "PG_USE_COPY", "YES");

        // Setting this option prevents GDAL from "cleaning" the table and column
        // names for Postgres, as described here:
        // https://gdal.org/drivers/vector/pg.html#layer-creation-options
        processLayerCreationOption(args, "LAUNDER", "NO");

        datasetCreationOptions.forEach((name, value) -> processDatasetCreationOption(args, name, value));
        layerCreationOptions.forEach((name, value) -> processLayerCreationOption(args, name, value));
        outputDatasetOpenOptions.forEach((name, value) -> processOutputDatasetOpenOption(args, name, value));
    }
}