package com.cmclinnovations.aermod.objects;

public class StaticPointSource extends PointSource {
    private String ocgmlIri;

    public StaticPointSource(String iri) {
        super(iri);
    }

    public void setOcgmlIri(String ocgmlIri) {
        this.ocgmlIri = ocgmlIri;
    }

    public String getOcgmlIri() {
        return ocgmlIri;
    }
}
