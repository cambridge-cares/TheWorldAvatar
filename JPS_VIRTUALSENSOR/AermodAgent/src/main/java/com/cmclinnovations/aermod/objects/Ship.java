package com.cmclinnovations.aermod.objects;

public class Ship extends PointSource {
    private String iri;

    public Ship(String iri) {
        this.iri = iri;
    }

    public String getIri() {
        return this.iri;
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