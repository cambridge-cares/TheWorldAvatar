package com.cmclinnovations.aermod.objects;

public class StaticPointSource extends PointSource {

    private String ocgmlIri;
    private String label = null;

    private CityObjectType pointSourceOCGMLType;

    public enum CityObjectType {
        CITY_FURNITURE, BUILDING
    }

    public StaticPointSource(String iri, CityObjectType cityObjType) {
        super(iri);
        pointSourceOCGMLType = cityObjType;
    }

    public StaticPointSource(String iri) {
        super(iri);
    }

    public void setOcgmlIri(String ocgmlIri) {
        this.ocgmlIri = ocgmlIri;
    }

    public String getOcgmlIri() {
        return ocgmlIri;
    }

    public void setCityObjectType(CityObjectType objType) {
        pointSourceOCGMLType = objType;
    }

    public CityObjectType getCityObjectType() {
        return pointSourceOCGMLType;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }
}
