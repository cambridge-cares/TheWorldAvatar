package com.cmclinnovations.stack.clients.geoserver;

import java.util.List;
import java.util.Map;

import it.geosolutions.geoserver.rest.encoder.feature.FeatureTypeAttribute;
import it.geosolutions.geoserver.rest.encoder.feature.GSAttributeEncoder;
import it.geosolutions.geoserver.rest.encoder.feature.GSFeatureTypeEncoder;

public class UpdatedGSFeatureTypeEncoder extends GSFeatureTypeEncoder {

    private UpdatedGSVirtualTableEncoder metadataVirtualTable;

    public UpdatedGSVirtualTableEncoder getMetadataVirtualTable() {
        return metadataVirtualTable;
    }

    public void setAttributes(List<Map<FeatureTypeAttribute, String>> attributes) {
        attributes.stream().forEach(map -> {
            GSAttributeEncoder attribute = new GSAttributeEncoder();
            attribute.setup(map);
            setAttribute(attribute);
        });
    }

    public void setMetadataVirtualTable(UpdatedGSVirtualTableEncoder metadataVirtualTable) {
        this.metadataVirtualTable = metadataVirtualTable;
    }

    public void setLatLonBoundingBox(GeoServerBoundingBoxSettings latLonBoundingBox) {
        super.setLatLonBoundingBox(latLonBoundingBox.getMinx(), latLonBoundingBox.getMinx(),
                latLonBoundingBox.getMaxx(), latLonBoundingBox.getMaxy(), latLonBoundingBox.getCrs());
    }

    public void setNativeBoundingBox(GeoServerBoundingBoxSettings nativeBoundingBox) {
        super.setNativeBoundingBox(nativeBoundingBox.getMinx(), nativeBoundingBox.getMinx(),
                nativeBoundingBox.getMaxx(), nativeBoundingBox.getMaxy(), nativeBoundingBox.getCrs());
    }
}
