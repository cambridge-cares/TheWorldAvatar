package uk.ac.cam.cares.jps.agent.cea.data;

import java.util.Map;

public class CEABuildingData {
    private CEAGeometryData geometry;
    private Map<String, Double> usage;

    public CEABuildingData(CEAGeometryData geometryValue, Map<String, Double> usageValue) {
        this.geometry = geometryValue;
        this.usage = usageValue;
    }

    public CEAGeometryData getGeometry() {
        return this.geometry;
    }

    public Map<String, Double> getUsage() {
        return this.usage;
    }

    public void setGeometry(CEAGeometryData geometryValue) {
        this.geometry = geometryValue;
    }

    public void setUsage(Map<String, Double> usageValue) {
        this.usage = usageValue;
    }
}
