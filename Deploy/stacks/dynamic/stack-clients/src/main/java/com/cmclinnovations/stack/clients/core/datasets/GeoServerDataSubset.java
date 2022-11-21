package com.cmclinnovations.stack.clients.core.datasets;

public abstract class GeoServerDataSubset extends PostgresDataSubset {

    @Override
    public boolean usesGeoServer() {
        return !isSkip();
    }

    @Override
    void loadInternal(Dataset parent) {
        super.loadInternal(parent);
        createLayer(parent.getWorkspaceName(), parent.getDatabase());
    }

    public abstract void createLayer(String workspaceName, String database);

}
