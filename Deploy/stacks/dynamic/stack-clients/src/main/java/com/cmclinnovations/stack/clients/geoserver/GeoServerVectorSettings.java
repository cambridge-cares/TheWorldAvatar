package com.cmclinnovations.stack.clients.geoserver;

import java.util.Map;

import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class GeoServerVectorSettings extends GSLayerEncoder implements GeoServerDimensionSettings {

    private UpdatedGSVirtualTableEncoder virtualTable;

    public GSVirtualTableEncoder getVirtualTable() {
        return virtualTable;
    }

    public void setVirtualTable(UpdatedGSVirtualTableEncoder virtualTable) {
        this.virtualTable = virtualTable;
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
