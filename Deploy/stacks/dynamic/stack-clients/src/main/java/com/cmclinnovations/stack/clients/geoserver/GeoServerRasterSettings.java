package com.cmclinnovations.stack.clients.geoserver;

import java.util.Map;

import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.coverage.GSImageMosaicEncoder;

public class GeoServerRasterSettings implements GeoServerDimensionSettings {

    private final GSImageMosaicEncoder dataStoreSettings = new GSImageMosaicEncoder();
    private final GSLayerEncoder layerSettings = new GSLayerEncoder();

    public GSImageMosaicEncoder getDataStoreSettings() {
        return dataStoreSettings;
    }

    public GSLayerEncoder getLayerSettings() {
        return layerSettings;
    }

    private Map<String, UpdatedGSFeatureDimensionInfoEncoder> dimensions;

    @Override
    public Map<String, UpdatedGSFeatureDimensionInfoEncoder> getDimensions() {
        return dimensions;
    }

    @Override
    public void setDimensions(Map<String, UpdatedGSFeatureDimensionInfoEncoder> dimensions) {
        this.dimensions = dimensions;
    }

}
