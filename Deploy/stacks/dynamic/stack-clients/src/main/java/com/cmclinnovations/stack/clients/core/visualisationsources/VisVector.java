package com.cmclinnovations.stack.clients.core.visualisationsources;

import com.cmclinnovations.stack.clients.core.datasets.Dataset;
import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSFeatureTypeEncoder;

import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder21;

public class VisVector extends VisSource {

    private final GSLayerEncoder21 layerSettings = new GSLayerEncoder21();
    private final UpdatedGSFeatureTypeEncoder featureTypeSettings = new UpdatedGSFeatureTypeEncoder();

    private String storeName;
    private String layerName;

    public GSLayerEncoder21 getLayerSettings() {
        return layerSettings;
    }

    public UpdatedGSFeatureTypeEncoder getFeatureTypeSettings() {
        return featureTypeSettings;
    }

    public String getStoreName() {
        return storeName;
    }

    public String getLayerName() {
        return layerName;
    }

    public void loadInternal(Dataset dataset) {
        GeoServerClient.getInstance().createVectorVisSource(dataset.getWorkspaceName(), getLayerName(), getStoreName(),
                featureTypeSettings,
                layerSettings);
    }
}
