package com.cmclinnovations.aermod.objects;

public class StaticPointSource extends PointSource {
    private String ocgmlIri;
    private double baseElevation;

    public StaticPointSource(String iri) {
        super(iri);
    }

    public void setOcgmlIri(String ocgmlIri) {
        this.ocgmlIri = ocgmlIri;
    }

    public String getOcgmlIri() {
        return ocgmlIri;
    }

    public void setElevation(double elevation) {
        this.baseElevation = elevation;
    }

    public double getElevation() {
        return baseElevation;
    }
}
