package uk.ac.cam.cares.jps.agent.cea.data;

import org.locationtech.jts.geom.Geometry;

import java.util.List;

public class CEAGeometryData {
    public List<Geometry> footprint;
    public String crs;
    public String height;

    public CEAGeometryData() {
        this.footprint = null;
        this.crs = null;
        this.height = null;
    }


    public CEAGeometryData(List<Geometry> footprint_value, String crs_value, String height_value) {
        this.footprint = footprint_value;
        this.crs = crs_value;
        this.height = height_value;
    }

    public void setFootprint(List<Geometry> footprint_value) {
        this.footprint = footprint_value;
    }
    public void setCrs(String crs_value) {
        this.crs = crs_value;
    }

    public void setHeight(String height_value) {
        this.height = height_value;
    }


    public List<Geometry> getFootprint() {
        return this.footprint;
    }
    public String getCrs() {
        return this.crs;
    }
    public String getHeight() {
        return this.height;
    }
}
