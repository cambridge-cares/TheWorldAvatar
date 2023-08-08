package com.cmclinnovations.aermod.objects;

public class Ship extends PointSource {
    public Ship(String iri) {
        super(iri);
    }

    @Override
    public double getDiameter() {
        return 1;
    }

    @Override
    public double getHeight() {
        return 20;
    }

    @Override
    public double getMixtureMassFlux() {
        return 0.0192143028723584;
    }
}