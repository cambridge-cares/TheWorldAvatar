package com.cmclinnovations.stack.clients.geoserver;

import java.util.Map;

import org.jdom.Element;

import it.geosolutions.geoserver.rest.encoder.metadata.GSFeatureDimensionInfoEncoder;

public class UpdatedGSFeatureDimensionInfoEncoder extends GSFeatureDimensionInfoEncoder {

    private static final String NEAREST_MATCH_ENABLED = "nearestMatchEnabled";
    private static final String RAW_NEAREST_MATCH_ENABLED = "rawNearestMatchEnabled";
    private static final String START_VALUE = "startValue";
    private static final String END_VALUE = "endValue";

    public UpdatedGSFeatureDimensionInfoEncoder() {
        super("");
    }

    private Map<String, String> defaultValue;
    private Boolean nearestMatchEnabled;
    private Boolean rawNearestMatchEnabled;
    private String startValue;
    private String endValue;

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
        set(NEAREST_MATCH_ENABLED, nearestMatchEnabled.toString());

    }

    public Boolean getRawNearestMatchEnabled() {
        return rawNearestMatchEnabled;
    }

    public void setRawNearestMatchEnabled(Boolean rawNearestMatchEnabled) {
        this.rawNearestMatchEnabled = rawNearestMatchEnabled;
        set(RAW_NEAREST_MATCH_ENABLED, rawNearestMatchEnabled.toString());
    }

    public void setUnits(String units) {
        setUnit(units);
    }

    public String getStartValue() {
        return startValue;
    }

    public void setStartValue(String startValue) {
        this.startValue = startValue;
        set(START_VALUE, startValue);
    }

    public String getEndValue() {
        return endValue;
    }

    public void setEndValue(String endValue) {
        this.endValue = endValue;
        set(END_VALUE, endValue);
    }
}
