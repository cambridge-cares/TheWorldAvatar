package com.cmclinnovations.aermod.objects;

import org.locationtech.jts.geom.LinearRing;

public class Building {
    private LinearRing footPrint;
    private double height;
    private String srid;

    public Building(LinearRing footPrint, double height, String srid) {
        this.footPrint = footPrint;
        this.height = height;
        this.srid = srid;
    }

    public LinearRing getFootprint() {
        return footPrint;
    }

    public double getHeight() {
        return height;
    }

    public String getSrid() {
        return srid;
    }
}
