package uk.ac.cam.cares.jps.agent.cea.data;

import java.util.Map;

public class CEABuildingData {
    public CEAGeometryData geometry;
    public Map<String, Double> usage;

    public CEABuildingData(CEAGeometryData geometry_value, Map<String, Double> usage_value) {
        this.geometry = geometry_value;
        this.usage = usage_value;
    }

    public CEAGeometryData getGeometry() {
        return this.geometry;
    }

    public Map<String, Double> getUsage() {
        return this.usage;
    }

    public void setGeometry(CEAGeometryData geometry_value) {
        this.geometry = geometry_value;
    }

    public void setUsage(Map<String, Double> usage_value) {
        this.usage = usage_value;
    }
}
