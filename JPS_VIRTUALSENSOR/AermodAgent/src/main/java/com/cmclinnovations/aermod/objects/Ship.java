package com.cmclinnovations.aermod.objects;

public class Ship {
    private String iri;
    private Chimney chimney;
    
    public Ship (String iri) {
        this.iri = iri;
        this.chimney = new Chimney();
    }

    public String getIri() {
        return this.iri;
    }

    public Chimney getChimney() {
        return this.chimney;
    }
}