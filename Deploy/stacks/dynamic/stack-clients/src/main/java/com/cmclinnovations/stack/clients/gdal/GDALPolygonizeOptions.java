package com.cmclinnovations.stack.clients.gdal;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GDALPolygonizeOptions extends VectorOptions<GDALPolygonizeOptions> {

    public GDALPolygonizeOptions() {
        super("gdal_polygonize");
    }

    @JsonProperty
    private final String fieldName = null;

    private String layerName = null;

    @Override
    protected void processSourceAndDestination(String source, String destination, List<String> args) {
        args.add(source);
        args.add(destination);
        if (null != layerName) {
            args.add(layerName);
        }
        if (null != fieldName) {
            args.add(fieldName);
        }
    }

    @Override
    protected void processLayerNameOption(String layerName, List<String> args) {
        this.layerName = layerName;
    }
}