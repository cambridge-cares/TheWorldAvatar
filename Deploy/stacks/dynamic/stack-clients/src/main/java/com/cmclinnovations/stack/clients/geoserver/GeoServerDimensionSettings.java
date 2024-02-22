package com.cmclinnovations.stack.clients.geoserver;

import java.util.Map;

public interface GeoServerDimensionSettings {

    Map<String, UpdatedGSFeatureDimensionInfoEncoder> getDimensions();

    void setDimensions(Map<String, UpdatedGSFeatureDimensionInfoEncoder> dimensions);

}