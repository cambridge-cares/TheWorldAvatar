package com.cmclinnovations.stack.clients.geoserver;

import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.coverage.GSImageMosaicEncoder;

public class GeoServerRasterSettings {

    private final GSImageMosaicEncoder dataStoreSettings = new GSImageMosaicEncoder();
    private final GSLayerEncoder layerSettings = new GSLayerEncoder();

    public GSImageMosaicEncoder getDataStoreSettings() {
        return dataStoreSettings;
    }

    public GSLayerEncoder getLayerSettings() {
        return layerSettings;
    }

}
