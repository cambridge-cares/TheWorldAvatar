package com.cmclinnovations.stack.clients.geoserver;

import it.geosolutions.geoserver.rest.encoder.GSLayerEncoder;
import it.geosolutions.geoserver.rest.encoder.metadata.virtualtable.GSVirtualTableEncoder;

public class GeoServerVectorSettings extends GSLayerEncoder {

    private UpdatedGSVirtualTableEncoder virtualTable;

    public GSVirtualTableEncoder getVirtualTable() {
        return virtualTable;
    }

    public void setVirtualTable(UpdatedGSVirtualTableEncoder virtualTable) {
        this.virtualTable = virtualTable;
    }
}
