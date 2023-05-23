package com.cmclinnovations.aermod.objects;

import org.postgis.Point;

public class Ship extends PointSource {
    private String iri;
    private Point location;

    public Ship(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return this.iri;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public Point getLocation() {
        return this.location;
    }

    @Override
    public double getDiameter() {
        return 1;
    }

    @Override
    public double getHeight() {
        return 20;
    }
}