package com.cmclinnovations.aermod.objects;

public class StaticPointSource extends PointSource {

    private String ocgmlIri;

    private double baseElevation = 0.0;
    private CityObjectType pointSourceOCGMLType;

    public enum CityObjectType {
        CITY_FURNITURE, BUILDING
    }

    public StaticPointSource(String iri, CityObjectType cityObjType) {
        super(iri);
        pointSourceOCGMLType = cityObjType;
    }

    public void setOcgmlIri(String ocgmlIri) {
        this.ocgmlIri = ocgmlIri;
    }

    public String getOcgmlIri() {
        return ocgmlIri;
    }

    public String getCityObjectIri() {
        return ocgmlIri.replace("cityfurniture", "cityobject").replace("building", "cityobject");

    }

    public void setElevation(double elevation) {
        this.baseElevation = elevation;
    }

    public double getElevation() {
        return baseElevation;
    }

    public void setCityObjectType(CityObjectType objType) {
        pointSourceOCGMLType = objType;
    }

    public CityObjectType getCityObjectType() {
        return pointSourceOCGMLType;
    }
}
