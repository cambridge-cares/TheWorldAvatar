package uk.ac.cam.cares.jps.agent.cea.data;

import org.locationtech.jts.geom.Geometry;

import java.util.List;

public class CEAGeometryData {
    public List<Geometry> footprint;
    public String height;
    public String crs;

    public CEAGeometryData(List<Geometry> footprint_value, String height_value, String crs_value) {
        this.footprint = footprint_value;
        this.height = height_value;
        this.crs = crs_value;
    }

    public void setFootprint(List<Geometry> footprint_value) {
        this.footprint = footprint_value;
    }

    public void setHeight(String height_value) {
        this.height = height_value;
    }

    public void setCrs(String crs_value) {
        this.crs = crs_value;
    }

    public List<Geometry> getFootprint() {
        return this.footprint;
    }

    public String getHeight() {
        return this.height;
    }

    public String getCrs() {
        return this.crs;
    }
}
