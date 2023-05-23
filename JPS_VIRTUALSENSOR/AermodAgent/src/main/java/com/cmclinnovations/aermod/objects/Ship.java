package com.cmclinnovations.aermod.objects;

import org.postgis.Point;

public class Ship {
    private String iri;
    private Chimney chimney;
    private Point location;

    public Ship(String iri) {
        this.iri = iri;
        this.chimney = new Chimney();
    }

    public String getIri() {
        return this.iri;
    }

    public Chimney getChimney() {
        return this.chimney;
    }

    public void setLocation(Point location) {
        this.location = location;
    }

    public Point getLocation() {
        return this.location;
    }
}