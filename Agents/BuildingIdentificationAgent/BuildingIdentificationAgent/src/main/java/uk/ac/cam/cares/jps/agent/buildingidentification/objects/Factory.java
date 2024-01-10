package uk.ac.cam.cares.jps.agent.buildingidentification.objects;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Point;

public class Factory {

    public String factoryClass = null;
    public String factoryIri = null;
    public String companyName = null;
    public double heatEmission = 0.0;
    public Point location = null;
    public Integer buildingId = -1;
    public Geometry buildingFootprint = null;
    public double buildingHeight = 0.0;

    public Factory(String factoryClass, String factoryIri, String companyName, Double heatEmission, Point location) {
        this.factoryClass = factoryClass;
        this.factoryIri = factoryIri;
        this.companyName = companyName;
        this.heatEmission = heatEmission;
        this.location = location;
    }

}
