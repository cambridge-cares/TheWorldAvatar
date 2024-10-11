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

}