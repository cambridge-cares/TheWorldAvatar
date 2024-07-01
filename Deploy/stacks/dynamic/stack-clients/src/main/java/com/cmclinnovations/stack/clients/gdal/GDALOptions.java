package com.cmclinnovations.stack.clients.gdal;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonProperty;

public abstract class GDALOptions<T extends GDALOptions<T>> extends CommonOptions<T> {

    @JsonProperty
    protected final Map<String, String> creationOptions = new LinkedHashMap<>();

    protected GDALOptions(String command) {
        super(command);
    }

    public final T addCreationOption(String name, String value) {
        creationOptions.put(name, value);
        return (T) this;
    }

    public final String[] generateCommand(String sourceFormat, String source, String destination,
            String... extraArgs) {

        List<String> args = new ArrayList<>();

        args.add("-if");
        args.add(sourceFormat);
        // https://gdal.org/drivers/raster/cog.html#raster-cog
        args.add("-of");
        args.add("COG");

        return generateCommandInternal(args, source, destination, extraArgs);
    }

    @Override
    protected void processArgs(final List<String> args) {
        super.processArgs(args);

        creationOptions.forEach((name, value) -> processKeyValuePair(args, "-co", name, value));
    }

}
