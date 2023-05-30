package com.cmclinnovations.aermod.objects;

import org.locationtech.jts.geom.LinearRing;
import org.locationtech.jts.geom.Point;
import org.postgis.Polygon;

public class Building {
    private LinearRing footPrint;
    private double height;
    private Point location;
    private double baseElevation = 0.0;

    public Building(LinearRing footPrint, double height) {
        this.footPrint = footPrint;
        this.height = height;
        this.location = footPrint.getCentroid();
    }

    public LinearRing getFootprint() {
        return footPrint;
    }

    public double getHeight() {
        return height;
    }

    public String getSrid() {
        return "EPSG:" + footPrint.getSRID();
    }

    public void setElevation(double elevation) {
        this.baseElevation = elevation;
    }

    public double getElevation() {
        return baseElevation;
    }

    public Point getLocation() {
        return location;
    }

}
