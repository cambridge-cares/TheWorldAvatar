package com.cmclinnovations.stack.clients.geoserver;

import java.util.Map;

import org.jdom.Element;

import it.geosolutions.geoserver.rest.encoder.metadata.GSFeatureDimensionInfoEncoder;

public class UpdatedGSFeatureDimensionInfoEncoder extends GSFeatureDimensionInfoEncoder {

    public UpdatedGSFeatureDimensionInfoEncoder() {
        super("");
    }

    private Map<String, String> defaultValue;

    private Boolean nearestMatchEnabled;

    private Boolean rawNearestMatchEnabled;

    public Map<String, String> getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(Map<String, String> defaultValue) {
        Element defaultValueNode = new Element("defaultValue");
        defaultValue.entrySet()
                .forEach(entry -> defaultValueNode.addContent(new Element(entry.getKey()).setText(entry.getValue())));
        addContent(defaultValueNode);
    }

    public Boolean getNearestMatchEnabled() {
        return nearestMatchEnabled;
    }

    public void setNearestMatchEnabled(Boolean nearestMatchEnabled) {
        this.nearestMatchEnabled = nearestMatchEnabled;
    }

    public Boolean getRawNearestMatchEnabled() {
        return rawNearestMatchEnabled;
    }

    public void setRawNearestMatchEnabled(Boolean rawNearestMatchEnabled) {
        this.rawNearestMatchEnabled = rawNearestMatchEnabled;
    }

    public void setUnits(String units) {
        setUnit(units);
    }

}
